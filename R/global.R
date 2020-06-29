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

#' Change the sign of the losses in a layer.
#' @param object the layer to change the sign of
#' @examples
#' test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' minus(test_layer)
#' @export
minus <- function(object) UseMethod("minus")


#' Compute value at risk for the losses in the layer.
#' @param object the layer or portfolio to computer VaR with.
#' @param rp_years Number of years in the return period
#' @param type AEP (aggregate exceedance probability)or OEP (occurrence exceedance probability). Defaults to AEP.
#' @examples
#' gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' VaR(gross_layer, 25)
#' VaR(gross_layer, 25, "AEP") # the same thing
#' VaR(gross_layer, 25, "OEP")
#' @export
VaR <- function(object, rp_years, type = c("AEP", "OEP")) UseMethod("VaR")


#' Compute tail value at risk for the losses in the layer.
#' @param object the layer or portfolio to computer VaR with.
#' @param rp_years Number of years in the return period
#' @param type AEP (aggregate exceedance probability)or OEP (occurrence exceedance probability). Defaults to AEP.
#' @examples
#' gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
#' tVaR(gross_layer, 25)
#' tVaR(gross_layer, 25, "AEP") # the same thing
#' tVaR(gross_layer, 25, "OEP")
#' @export
tVaR <- function(object, rp_years, type = c("AEP", "OEP")) UseMethod("tVaR")


#' A function to calculate Average Annual Loss (AAL) from a yelt
#' @param yelt The yelt as a data frame or tibble. It must contain columns named trialID, LOB, and Loss
#' @examples
#' AAL(yelt_test)
#' @export
AAL <- function(yelt, all=FALSE) {
  stopifnot(all(c("trialID", "LOB", "Loss") %in% names(yelt)))
  n_trials <- length(unique(yelt$trialID))
  ans <- yelt %>% group_by(.data$LOB) %>%
    summarise(loss = sum(.data$Loss)/n_trials, .groups = "drop")
  if (all) ans <- sum(ans$loss)
  return(ans)
}
