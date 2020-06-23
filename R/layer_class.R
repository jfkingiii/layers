#' Convenient synonym for .Machine$double.xmax
#' @export
UNLIMITED <- .Machine$double.xmax

#' Create a layer object.
#'
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
#' layer(4000000, 1000000, 1, "yelt", c("GL", "AUTO"), 0, UNLIMITED)

# Experimenting:
# losses <-
#   get(loss_set) %>% filter(LOB %in% lobs) %>% select(trialID, Loss)
# losses$ceded_loss <-
#   pmin(pmax(losses$Loss - attachment, 0), limit) * participation
# trial_results <-
#   losses %>% group_by(trialID) %>% summarise(gross_loss = sum(Loss), ceded_loss = sum(ceded_loss), .groups = "drop")
# trial_results$ceded_loss <-
#   pmin(pmax(trial_results$ceded_loss - layer$agg_attachment, 0),
#        layer$agg_limit)


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
    value <-
      list(
        attachment = attachment,
        limit = limit,
        participation = participation,
        loss_set = loss_set,
        lobs = lobs,
        agg_attachment = agg_attachment,
        agg_limit = agg_limit
      )
    class(value) <- "layer"
    return(value)
  }


#' Print function for objects of class layer.
#' @examples
#' test_layer
#' print(test_layer)
#' @export
print.layer <- function(layer) {
  attachment <- format(layer$attachment, big.mark = ",", scientific = FALSE)
  limit <-  format(layer$limit, big.mark = ",", scientific = FALSE)
  participation <- format(layer$participation, nsmall=3, format="f")
  cat("Limit:\t\t", limit, "\n")
  cat("Attachment:\t", attachment, "\n")
  cat("Participation:\t", participation, "\n")
  if (layer$agg_attachment != 0 | layer$agg_limit != UNLIMITED)
  {
    agg_attachment <- format(layer$agg_attachment, big.mark = ",", scientific = FALSE)
    agg_limit <-  format(layer$agg_limit, big.mark = ",", scientific = FALSE)
    cat("Agg Attachment:\t", agg_attachment, "\n")
    cat("Agg Limit:\t", agg_limit, "\n")
  }
  if (!is.null(layer$lobs))
    cat("Loss set:\t", layer$loss_set, "\n")
    cat("LOBs:\t\t", layer$lobs, "\n")
}


#' Compute a list of metrics for the layer.
#'
#' @param layer The layer to calculate metrics for.
#' @return An object of class metric_list containing mean, standard deviation, VaR and tVaR (AEP).
#' @export
#' @examples
#' layer(4000000, 1000000, 1, "yelt", c("GL", "AUTO"), 0, UNLIMITED)
metrics <- function(layer) {
  UseMethod("metrics")
}

#' @import dplyr
#' @rdname metrics
#' @export
metrics.layer <- function(layer) {
  losses <-
    get(layer$loss_set) %>% filter(LOB %in% layer$lobs) %>% select(trialID, Loss)
  losses$ceded_loss <-
    pmin(pmax(losses$Loss - layer$attachment, 0), layer$limit) * layer$participation
  trial_results <-
    losses %>% group_by(trialID) %>% summarise(ceded_loss = sum(ceded_loss), .groups = "drop")
  trial_results$ceded_loss <-
    pmin(pmax(trial_results$ceded_loss - layer$agg_attachment, 0),
         layer$agg_limit)
  ceded <- trial_results$ceded_loss
  ans <- list(
    mean = mean(ceded),
    sd = sd(ceded),
    var25 = quantile(ceded, 1 - 1/25),
    var100 = quantile(ceded, 1 - 1/100),
    var250 = quantile(ceded, 1 - 1/250)
  )
  tvar25 <- mean(ceded[ceded > ans$var25])
  tvar100 <- mean(ceded[ceded > ans$var100])
  tvar250 <- mean(ceded[ceded > ans$var250])
  ans <- c(ans, tvar25=tvar25, tvar100=tvar100, tvar250=tvar250)
  class(ans) <- "metric_list"
  return(ans)
}


#' Print function for objects of class metric_list
#' @examples
#' metrics(example_layer)
#' print(metrics(example_layer))
#' @export
print.metric_list <- function(x) {
  z <- lapply(x, function(y) format(round(y), big.mark = ",", scientific = FALSE))
  cat("Mean:\t\t", z$mean, "\n")
  cat("StdDev:\t\t", z$sd, "\n")
  cat("VaR 25:\t\t", z$var25, "\n")
  cat("VaR 100:\t", z$var100, "\n")
  cat("VaR 250:\t", z$var250, "\n")
  cat("tVaR 25:\t", z$tvar25, "\n")
  cat("tVaR 100:\t", z$tvar100, "\n")
  cat("tVaR 250:\t", z$tvar250, "\n")
}
