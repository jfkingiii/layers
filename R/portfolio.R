#' Explodes a list of porfolios into a list of layers.
#' @param L the layers to be exploded
#' @return A list containing all the layers in L
#' @export
explode <- function(L){
  unlist(lapply(L, function(x) x$layer_list), recursive = FALSE)
}

# Need a slot in the layer object for sign (+ or -) ??

#' Create a portfolio object.
#' @param ... layers in the portfolio
#' @return The portfolio object.
#' @export
portfolio <- function(...) {
  layer_list <- list(...)
  # make sure all the arguments are layers
  # TODO change portfolio so that a portfolio can be an argument
  # e.g. net = portfolio(gross, minus(ceded))
  stopifnot(is.list(layer_list),
            all(sapply(layer_list, function(x)
              class(x) %in% c("layer", "portfolio"))))
  layer_ind <- sapply(layer_list, is, "layer")
  port_ind <- !layer_ind
  port_list <- layer_list[port_ind]
  layer_list <- c(layer_list[layer_ind], explode(port_list))
  # Test that the loss sets are the same for every layer
  lsnames <- unique(sapply(layer_list, function(x) x$loss_set))
  stopifnot(length(lsnames) == 1)
  trial_results <-
    lapply(layer_list, function(layer) layer$trial_results) %>%
    bind_rows()
  trial_results <-
    trial_results %>% group_by(.data$trialID) %>% summarise(
      ceded_loss = sum(.data$ceded_loss), .groups = "drop")
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
#' @export print.portfolio
#' @export
print.portfolio <- function(x, ...) {
  for (layer in x$layer_list) {
    print(layer)
    cat("\n")
  }
}


#' @rdname expected
#' @export expected.portfolio
#' @export
expected.portfolio <- function(object)
  return(mean(object$trial_results$ceded_loss))


#' @rdname stdev
#' @export stdev.portfolio
#' @export
stdev.portfolio <- function(object){
  #return(sd(trials$ceded_loss))
  return(sd(object$trial_results$ceded_loss))
}


#' @rdname minus
#' @export minus.layer
#' @export
minus.portfolio <- function(object){
  minus_list <- lapply(object$layer_list, minus)
  return(do.call(portfolio, minus_list))
}


#' @rdname VaR
#' @export VaR.portfolio
#' @export
VaR.portfolio <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  stopifnot(type == "AEP") # OEP not working for portfolios
  aep_sort <- sort(object$trial_results$ceded_loss, decreasing = TRUE)
  ans <- aep_sort[nrow(object$trial_results) / rp_years]
  return(unname(ans))
}


#' @rdname tVaR
#' @export tVaR.portfolio
#' @export
tVaR.portfolio <- function(object, rp_years, type = c("AEP", "OEP")) {
  type = match.arg(type)
  stopifnot(type == "AEP") # OEP not working for portfolios
  v <- VaR(object = object, rp_years = rp_years, type = type)
  aep <- object$trial_results$ceded_loss
  ans <- mean(aep[aep >= v])
  return(unname(ans))
}
