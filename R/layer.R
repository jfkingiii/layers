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
    # The loss set is expected to have columns named LOB, trialID, and Loss
    stopifnot(all(c("LOB", "trialID", "Loss") %in% names(get(loss_set))))
    valid_lobs <- unique(get(loss_set)$LOB)
    stopifnot(all(lobs %in% valid_lobs))
    # Layer object will store the trial_results data
    losses <-
      get(loss_set) %>% filter(.data$LOB %in% lobs) %>% select(.data$trialID, .data$Loss)
    losses$layered_loss <-
      pmin(pmax(losses$Loss - attachment, 0), limit)
    trial_results <-
      losses %>% group_by(.data$trialID) %>% summarise(
        ceded_loss = sum(.data$layered_loss),
        max_ceded_loss = max(.data$layered_loss),
        .groups = "drop"
      )
    trial_results$ceded_loss <-
      pmin(pmax(trial_results$ceded_loss - agg_attachment, 0),
           agg_limit) * participation
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
    mn <- min(trial_results$ceded_loss)
    mx <- max(trial_results$ceded_loss)
    if (mn < 0 & mx > 0) stop("Mixed signs in layer losses")
    return(value)
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
  # fix this to handle agg limit and agg deductible correctly
  attachment <- format(x$attachment, big.mark = ",", scientific = FALSE)
  if (x$limit == UNLIMITED) limit <- "UNLIMITED"
  else limit <-  format(x$limit, big.mark = ",", scientific = FALSE)
  participation <- format(x$participation, nsmall=3, format="f")
  sgn <- min(x$trial_results$ceded_loss)
  if (sgn >= 1) layer_sign <- "+"
  else layer_sign <- "-"
  df <- data.frame(
    row.names = c("Limit:", "Attachment:", "Participation:", "Loss set:", "LOBs:", "Sign:"),
    Value = c(limit, attachment, participation, x$loss_set, paste(x$lobs, collapse=" "), layer_sign),
    stringsAsFactors = FALSE)
  if (x$agg_attachment != 0 | x$agg_limit != UNLIMITED)
  {
    agg_attachment <- format(x$agg_attachment, big.mark = ",", scientific = FALSE)
    agg_limit <-  format(x$agg_limit, big.mark = ",", scientific = FALSE)
    df2 <-
      data.frame(
        row.names = c("Agg Attachment:", "Agg Limit:"),
        Value = c(agg_attachment, agg_limit),
        stringsAsFactors = FALSE
      )
    df <- rbind(df, df2)
  }
  print(df)
}


#' @rdname expected
#' @export expected.layer
#' @export
expected.layer <- function(object)
# TODO what if some of the trials have no losses? then this mean will not work
    return(mean(object$trial_results$ceded_loss))

#' @rdname stdev
#' @export stdev.layer
#' @export
stdev.layer <- function(object)
    return(sd(object$trial_results$ceded_loss))


#' @rdname minus
#' @export minus.layer
#' @export
minus.layer <- function(object){
  object$trial_results$ceded_loss <- (-object$trial_results$ceded_loss)
  object$trial_results$max_ceded_loss <- (-object$trial_results$max_ceded_loss)
  return(object)
}


#' @rdname VaR
#' @export VaR.layer
#' @export
VaR.layer <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  if (type == "AEP") {
    aep_sort <- sort(object$trial_results$ceded_loss, decreasing = TRUE)
    ans <- aep_sort[nrow(object$trial_results)/rp_years]
  }
  else if (type == "OEP") {
    oep_sort <- sort(object$trial_results$max_ceded_loss, decreasing = TRUE)
    ans <- oep_sort[nrow(object$trial_results)/rp_years]
  }
  return(unname(ans))
}


#' @rdname tVaR
#' @export tVaR.layer
#' @export
tVaR.layer <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  v <- VaR(object = object, rp_years = rp_years, type = type)
  if (type == "AEP") {
    aep <- object$trial_results$ceded_loss
    ans <- mean(aep[aep >= v])
  }
  else if (type == "OEP") {
    oep <- object$trial_results$max_ceded_loss
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
    Value = z,
    stringsAsFactors = FALSE
  ))
}
