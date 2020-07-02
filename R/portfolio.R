#' Explodes a list of porfolios into a list of layers.
#' @param L list of portfolios to be exploded
#' @return A list containing all the layers in L
#' @export
explode <- function(L){
  unlist(lapply(L, function(x) x$layer_list), recursive = FALSE)
}


#' Create a portfolio object.
#' @param ... layers or portfolios (a portfolio can be an argument to portfolio)
#' @return The portfolio object.
#' @export
portfolio <- function(...) {
  arg_list <- list(...)
  # make sure all the arguments are layers or portfolios
  # A portfolio can be an argument to portfolio
  # e.g. net = portfolio(gross, minus(ceded))
  stopifnot(is.list(arg_list),
            all(sapply(arg_list, function(x)
              class(x) %in% c("layer", "portfolio"))))
  layer_ind <- sapply(arg_list, is, "layer")
  port_ind <- !layer_ind
  port_list <- arg_list[port_ind]
  layer_list <- c(arg_list[layer_ind], explode(port_list))
  # Test that the loss sets are the same for every layer
  lsnames <- unique(sapply(layer_list, function(x) x$loss_set))
  stopifnot(length(lsnames) == 1)
  trial_results <- lapply(layer_list, function(layer) layer$trial_results)
  trial_results <- do.call("rbind", trial_results)
  trial_results <- aggregate(trial_results["ceded_loss"], trial_results["trialID"], sum)
  ans <- list(layer_list = layer_list, trial_results = trial_results)
  class(ans) <- "portfolio"
  return(ans)
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
#' @export
print.portfolio <- function(x, ...) {
  for (layer in x$layer_list) {
    print(layer)
    cat("\n")
  }
}


#' @rdname expected
#' @export
expected.portfolio <- function(object)
  return(mean(object$trial_results$ceded_loss))


#' @rdname stdev
#' @export
stdev.portfolio <- function(object){
  #return(sd(trials$ceded_loss))
  return(sd(object$trial_results$ceded_loss))
}


#' @rdname minus
#' @export
minus.portfolio <- function(object){
  minus_list <- lapply(object$layer_list, minus)
  return(do.call(portfolio, minus_list))
}


#' @rdname VaR
#' @export
VaR.portfolio <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  if(type == "OEP") stop("OEP not implemented for portfolios")
  aep_sort <- sort(object$trial_results$ceded_loss, decreasing = TRUE)
  ans <- aep_sort[nrow(object$trial_results) / rp_years]
  return(unname(ans))
}


#' @rdname tVaR
#' @export
tVaR.portfolio <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  if(type == "OEP") stop("OEP not implemented for portfolios") # OEP not working for portfolios
  v <- VaR(object = object, rp_years = rp_years, type = type)
  aep <- object$trial_results$ceded_loss
  ans <- mean(aep[aep >= v])
  return(unname(ans))
}


#' Summarize the portfolio parameters, and compute some metrics
#' for the portfolio
#' @param object The portfolio to calculate metrics for.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @return An object of class summary.portfolio containing layer parameters, mean,
#' standard deviation, VaR and tVaR (AEP).
#' @examples
#' layer1 <- layer(100000, 100000, 1, "yelt_test", lobs="PHYSICIANS")
#' layer2 <- layer(100000, 200000, 1, "yelt_test", lobs="PHYSICIANS")
#' layer3 <- layer(100000, 300000, 1, "yelt_test", lobs="PHYSICIANS")
#' P <- portfolio(layer1, layer2, layer3)
#' summary(P)
#' @export
summary.portfolio <- function(object, ...) {
  ans <- list(portfolio = object,
              mean = expected(object),
              sd = stdev(object),
              var25 = VaR(object, 25, "AEP"),
              var100 = VaR(object, 100, "AEP"),
              var250 = VaR(object, 250, "AEP"),
              tvar25 = tVaR(object, 25, "AEP"),
              tvar100 = tVaR(object, 100, "AEP"),
              tvar250 = tVaR(object, 250, "AEP")
  )
  class(ans) <- "summary.portfolio"
  return(ans)
}


#' Print function for objects of class summary.portfolio
#' @param x The summary to be printed.
#' @param ... Objects to be passed to subsequent methods, if they existed.
#' @examples
#' layer1 <- layer(100000, 100000, 1, "yelt_test", lobs="PHYSICIANS")
#' layer2 <- layer(100000, 200000, 1, "yelt_test", lobs="PHYSICIANS")
#' layer3 <- layer(100000, 300000, 1, "yelt_test", lobs="PHYSICIANS")
#' P <- portfolio(layer1, layer2, layer3)
#' summary(P)
#' print(summary(P)) # same thing
#' @export
print.summary.portfolio <- function(x, ...) {
  print(x$portfolio)
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
