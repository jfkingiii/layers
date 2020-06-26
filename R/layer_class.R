#' @import dplyr
#' @importFrom stats quantile sd
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
    valid_lobs <- unique(get(loss_set)$LOB)
    stopifnot(all(lobs %in% valid_lobs))
#   Layer object will now store the trial_results data
    losses <-
      get(loss_set) %>% filter(LOB %in% lobs) %>% select(trialID, Loss)
    losses$ceded_loss <-
      pmin(pmax(losses$Loss - attachment, 0), limit) * participation
    trial_results <-
      losses %>% group_by(trialID) %>% summarise(ceded_loss = sum(ceded_loss), .groups = "drop")
    trial_results$ceded_loss <-
      pmin(pmax(trial_results$ceded_loss - agg_attachment, 0), agg_limit)
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
#'
#' @param layer_list a list of layer objects
#' @return The portfolio object.
#' @export
#' @examples
#' portfolio(list(layer1, layer2, layer3))
portfolio <- function(layer_list){
#   stopifnot(is.list(layer_list), all(sapply(layer_list, is, "layer")))
  return(NULL)
}

#' Print function for objects of class layer.
#' @param x The layer to be printed.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' test_layer
#' print(test_layer)
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

#' Compute the expected losses ceded to the layer.
#' @param layer the layer to compute the expectation of
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' expected(test_layer)
#' @export
expected <- function(layer) UseMethod("expected")

#' Compute the standard deviation of losses ceded to the layer.
#' @param layer the layer to compute the standard deviation of
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' stdev(test_layer)
#' @export
stdev <- function(layer) UseMethod("stdev")

#' Compute value at risk for the losses in the layer.
#' @param layer the layer to computer VaR with.
#' @param q Quantile for VaR. For example, if the return period is 100 years, q = 1 - 1/100.
#' @param type AEP (aggregate exceedance probability)or OEP (occurrence exceedance probability). Defaults to AEP.
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' VaR(test_layer, 1 - 1/25)
#' VaR(test_layer, 1 - 1/25, "AEP") # the same thing
#' VaR(test_layer, 1 - 1/25, "OEP")
#' @export
VaR <- function(layer, q, type = c("AEP", "OEP")) UseMethod("VaR")

#' Compute tail value at risk for the losses in the layer.
#' @param layer the layer to computer tVaR with.
#' @param q Quantile for tVaR. For example, if the return period is 100 years, q = 1 - 1/100.
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' tVaR(test_layer, 1 - 1/25)
#' @export
tVaR <- function(layer, q) UseMethod("tVaR")

#' @rdname expected
#' @export
expected.layer <- function(layer)
    return(mean(layer$trial_results$ceded_loss))

#' @rdname stdev
#' @export
stdev.layer <- function(layer)
    return(sd(layer$trial_results$ceded_loss))

#' @rdname VaR
#' @export
VaR.layer <- function(layer, q, type = c("AEP", "OEP")) {
  type = match.arg(type)
  if (type == "AEP") {
    ans <- return(quantile(layer$trial_results$ceded_loss, q))
  }
  else {
    x <- get(layer$loss_set)$Loss
    y <- pmin(pmax(x - layer$attachment, 0), layer$limit)*layer$participation
    ans <- quantile(y, q)
  }
  return(unname(ans))
}

#' @rdname tVaR
#' @export
tVaR.layer <- function(layer, q) {
  ceded <- layer$trial_results$ceded_loss
  ans <- mean(ceded[ceded >= VaR(layer, q)])
  return(unname(ans))
}

#'  Summarize the layer parameters, and compute some metrics
#'  for the layer.
#' @param object The layer to calculate metrics for.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @return An object of class summary.layer containing layer parameters, mean,
#' standard deviation, VaR and tVaR (AEP).
#' @export
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' summary(test_layer)
#' @export
summary.layer <- function(object, ...) {
  ans <- list(layer = object,
    mean = expected(object),
    sd = stdev(object),
    var25 = VaR(object, 1 - 1 / 25),
    var100 = VaR(object, 1 - 1 / 100),
    var250 = VaR(object, 1 - 1 / 250),
    tvar25 = tVaR(object, 1 - 1 / 25),
    tvar100 = tVaR(object, 1 - 1 / 100),
    tvar250 = tVaR(object, 1 - 1 / 250)
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
