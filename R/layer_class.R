#' @import dplyr
#' @importFrom stats quantile sd
#' @importFrom methods is
NULL

#' A sample YELT (year event loss table) to test layers functions on
#'
#' @format A data frame with 127648 rows and 5 variables:
#' \describe{
#'   \item{trialID}{Identifier for the trial, which represents one year}
#'   \item{LOB}{Line of business or other segmentation}
#'   \item{Loss}{Loss amount}
#'   \item{Sequence}{Sequence of loss within trial}
#'
#' }
"yelt_test"

#' Convenient synonym for .Machine$double.xmax.
#' @export
UNLIMITED <- .Machine$double.xmax


#' Create a layer object.
#' @param attachment layer per occurrence attachment point.
#' @param limit layer per occurrence limit.
#' @param participation layer participation (proportion ceded).
#' @param loss_set String containing the name of data frame. The dataframe must contain columns trialID, LOB, and Loss.
#' @param lobs list of LOBs the layer applies to.
#' @param agg_attachment layer per occurrence limit.
#' @param agg_limit layer per occurrence limit.
#' @return The layer object.
#' @export
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' agg_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"),
#'                    agg_attachment = 4000000, agg_limit=8000000)
layer <-
  function(limit,
           attachment,
           participation = 1,
           loss_set,
           lobs,
           agg_attachment = 0,
           agg_limit = UNLIMITED) {
    # References to .data in dplyr constructions are to avoid
    # "no visible binding for global variable" note when running BUILD check to check the package
    valid_lobs <- unique(get(loss_set)$LOB)
    stopifnot(all(lobs %in% valid_lobs))
    # Layer object will now store the trial_results data
    losses <-
      get(loss_set) %>% filter(.data$LOB %in% lobs) %>% select(.data$trialID, .data$Loss)
    losses$layered_loss <-
      pmin(pmax(losses$Loss - attachment, 0), limit) * participation
    trial_results <-
      losses %>% group_by(.data$trialID) %>% summarise(
        ceded_loss = sum(.data$layered_loss),
        max_ceded_loss = max(.data$layered_loss),
        .groups = "drop"
      )
    trial_results$ceded_loss <-
      pmin(pmax(trial_results$ceded_loss - agg_attachment, 0),
           agg_limit)
    value <-
      list(
        attachment = attachment,
        limit = limit,
        participation = participation,
        loss_set = loss_set,
        lobs = lobs,
        agg_attachment = agg_attachment,
        agg_limit = agg_limit,
        trial_results = trial_results
      )
    class(value) <- "layer"
    return(value)
  }

#' Create a portfolio object.
#' @param ... layers in the portfolio
#' @return The portfolio object.
#' @export
portfolio <- function(...){
  layer_list <- list(...)
  stopifnot(is.list(layer_list), all(sapply(layer_list, is, "layer")))
  class(layer_list) <- "portfolio"
  return(layer_list)
}

#' Print function for objects of class layer.
#' @param x The layer to be printed.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' test_layer
#' print(test_layer)
#' @export print.layer
#' @export
print.layer <- function(x, ...) {
  attachment <- format(x$attachment, big.mark = ",", scientific = FALSE)
  if (x$limit == UNLIMITED) limit <- "UNLIMITED"
  else limit <-  format(x$limit, big.mark = ",", scientific = FALSE)
  participation <- format(x$participation, nsmall=3, format="f")
  cat("Limit:\t\t", limit, "\n")
  cat("Attachment:\t", attachment, "\n")
  cat("Participation:\t", participation, "\n")
  if (x$agg_attachment != 0 | x$agg_limit != UNLIMITED)
  {
    agg_attachment <- format(x$agg_attachment, big.mark = ",", scientific = FALSE)
    agg_limit <-  format(x$agg_limit, big.mark = ",", scientific = FALSE)
    cat("Agg Attachment:\t", agg_attachment, "\n")
    cat("Agg Limit:\t", agg_limit, "\n")
  }
  cat("Loss set:\t", x$loss_set, "\n")
  cat("LOBs:\t\t", x$lobs, "\n")
}

#' Print function for objects of class portfolio.
#' @param x The layer to be printed.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @examples
#' layer1 <- layer(4000000, 1000000, 1, "yelt_test", lobs="PHYSICIANS")
#' layer2 <- layer(5000000, 5000000, 1, "yelt_test", lobs="PHYSICIANS")
#' P <- portfolio(layer1, layer2)
#' P
#' print(P)
#' @export print.portfolio
#' @export
print.portfolio <- function(x, ...){
  invisible(sapply(x, function(y) {print(y); cat("\n")}))
  }

#' Compute the expected losses ceded to the layer.
#' @param object the layer or portfolio to compute the expectation of
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' expected(test_layer)
#' @export
expected <- function(object) UseMethod("expected")

#' Compute the standard deviation of losses ceded to the layer.
#' @param object the layer or portfolio to compute the standard deviation of
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' stdev(test_layer)
#' @export
stdev <- function(object) UseMethod("stdev")

#' Compute value at risk for the losses in the layer.
#' @param layer the layer to computer VaR with.
#' @param rp_years Number of years in the return period
#' @param type AEP (aggregate exceedance probability)or OEP (occurrence exceedance probability). Defaults to AEP.
#' @examples
#' gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' VaR(gross_layer, 25)
#' VaR(gross_layer, 25, "AEP") # the same thing
#' VaR(gross_layer, 25, "OEP")
#' @export
VaR <- function(layer, rp_years, type = c("AEP", "OEP")) UseMethod("VaR")

#' Compute tail value at risk for the losses in the layer.
#' @param layer the layer to computer VaR with.
#' @param rp_years Number of years in the return period
#' @param type AEP (aggregate exceedance probability)or OEP (occurrence exceedance probability). Defaults to AEP.
#' @examples
#' gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' tVaR(gross_layer, 25)
#' tVaR(gross_layer, 25, "AEP") # the same thing
#' tVaR(gross_layer, 25, "OEP")
#' @export
tVaR <- function(layer, rp_years, type = c("AEP", "OEP")) UseMethod("tVaR")

#' @rdname expected
#' @export expected.layer
#' @export
expected.layer <- function(object)
    return(mean(object$trial_results$ceded_loss))

#' @rdname expected
#' @export expected.portfolio
#' @export
expected.portfolio <- function(object)
  return(sum(sapply(object, expected.layer)))

#' @rdname stdev
#' @export stdev.layer
#' @export
stdev.layer <- function(object)
    return(sd(object$trial_results$ceded_loss))

#' @rdname stdev
#' @export stdev.portfolio
#' @export
stdev.portfolio <- function(object)
  return(NULL)


#' @rdname VaR
#' @export VaR.layer
#' @export
VaR.layer <- function(layer, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  if (type == "AEP") {
    aep_sort <- sort(layer$trial_results$ceded_loss, decreasing = TRUE)
    ans <- aep_sort[nrow(layer$trial_results)/rp_years]
  }
  else if (type == "OEP") {
    oep_sort <- sort(layer$trial_results$max_ceded_loss, decreasing = TRUE)
    ans <- oep_sort[nrow(layer$trial_results)/rp_years]
  }
  return(unname(ans))
}

#' @rdname tVaR
#' @export tVaR.layer
#' @export
tVaR.layer <- function(layer, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  v <- VaR(layer = layer, rp_years = rp_years, type = type)
  if (type == "AEP") {
    aep <- layer$trial_results$ceded_loss
    ans <- mean(aep[aep >= v])
  }
  else if (type == "OEP") {
    oep <- layer$trial_results$max_ceded_loss
    ans <- mean(oep[oep >= v])
  }
  return(unname(ans))
}

#'  Summarize the layer parameters, and compute some metrics
#'  for the layer.
#' @param object The layer to calculate metrics for.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @return An object of class summary.layer containing layer parameters, mean,
#' standard deviation, VaR and tVaR (AEP).
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' summary(test_layer)
#' @export summary.layer
#' @export
summary.layer <- function(object, ...) {
  ans <- list(layer = object,
    mean = expected(object),
    sd = stdev(object),
    var25 = VaR(object, 25, "AEP"),
    var100 = VaR(object, 100, "AEP"),
    var250 = VaR(object, 250, "AEP"),
    tvar25 = tVaR(object, 25, "AEP"),
    tvar100 = tVaR(object, 100, "AEP"),
    tvar250 = tVaR(object, 250, "AEP")
  )
  class(ans) <- "summary.layer"
  return(ans)
}


#' Print function for objects of class summary.layer
#' @param x The summary to be printed.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' summary(test_layer)
#' print(summary(test_layer)) # same thing
#' @export print.summary.layer
#' @export
print.summary.layer <- function(x, ...) {
  print(x$layer)
  cat("\n")
  z <- sapply(x[-1], function(y) format(round(y), big.mark = ",", scientific = FALSE))
  names(z) <- NULL
  print(data.frame(
    row.names = c(
      "Mean:",
      "StdDev:",
      "VaR 25:",
      "VaR 100:",
      "VaR 250:",
      "tVaR 25:",
      "tVaR 100:",
      "tVaR 250:"
    ),
    Value = z
  ))
}
