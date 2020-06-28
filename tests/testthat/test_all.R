library(layers)

print("running test")

test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE","HOSPITAL"))
agg_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"),
                   agg_attachment = 4000000, agg_limit = 12000000)
layer1 <- layer(100000, 100000, 1, "yelt_test", lobs="PHYSICIANS")
layer2 <- layer(100000, 200000, 1, "yelt_test", lobs="PHYSICIANS")
layer3 <- layer(100000, 300000, 1, "yelt_test", lobs="PHYSICIANS")
test_layer <- layer(4000000, 1000000, 1, "yelt_test",
                    lobs=c("PHYSICIANS", "CHC", "MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test",
                     lobs=c("PHYSICIANS", "CHC", "MEDCHOICE", "HOSPITAL"))

P <- portfolio(layer1, layer2, layer3)

assign("yelt2", yelt_test[1:10000, ], pos = 1) # pos=1 so get can find yelt2
layer_different <- layer(100000, 100000, 1, "yelt2", lobs = "PHYSICIANS")

test_that("portfolio errors if layers have different loss sets", {
  expect_error(portfolio(layer2, layer_different))
})

test_that("yelt_test is OK", {
expect_equal(nrow(yelt_test), 127648)
})

test_that("expected is accurate", {
  expect_equal(trunc(expected(test_layer)), 6782624)
  expect_equal(trunc(expected(gross_layer)), 102235224)
  expect_equal(trunc(expected(agg_layer)), 3037403)
})

test_that("standard deviation is accurate", {
  expect_equal(trunc(stdev(test_layer)), 3381071)
  expect_equal(trunc(stdev(gross_layer)), 16176233)
  expect_equal(trunc(stdev(agg_layer)), 2995537)
})

test_that("AEP VaR is accurate", {
  expect_equal(trunc(VaR(test_layer, 5, "AEP")), 9450220)
  expect_equal(trunc(VaR(gross_layer, 25, "AEP")), 134029327)
  expect_equal(trunc(VaR(agg_layer, 50, "AEP")), 11141487)
})

test_that("OEP VaR is accurate", {
  expect_equal(trunc(VaR(gross_layer, 1, "OEP")), 2000000)
  expect_equal(trunc(VaR(gross_layer, 2, "OEP")), 8000000)
  expect_equal(trunc(VaR(gross_layer, 3, "OEP")), 10000000)
})

test_that("AEP tVaR is accurate", {
  expect_equal(trunc(tVaR(test_layer, 5, "AEP")), 11975440)
  expect_equal(trunc(tVaR(gross_layer, 25, "AEP")), 141894334)
  expect_equal(trunc(tVaR(agg_layer, 50, "AEP")), 11781907)
})

test_that("OEP tVaR is accurate", {
  expect_equal(trunc(tVaR(test_layer, 1, "OEP")), 2405781)
  expect_equal(trunc(tVaR(gross_layer, 5, "OEP")), 13254096)
  expect_equal(trunc(tVaR(agg_layer, 1, "OEP")), 2405781)
})

test_that("The layer summary function gives the right values", {
  expect_equal(summary(layer1)$mean, expected(layer1))
  expect_equal(summary(layer1)$sd, stdev(layer1))
  expect_equal(summary(layer1)$var25, VaR(layer1, 25, "AEP"))
  expect_equal(summary(layer1)$var100, VaR(layer1, 100, "AEP"))
  expect_equal(summary(layer1)$var250, VaR(layer1, 250, "AEP"))
  expect_equal(summary(layer1)$tvar25, tVaR(layer1, 25, "AEP"))
  expect_equal(summary(layer1)$tvar100, tVaR(layer1, 100, "AEP"))
  expect_equal(summary(layer1)$tvar250, tVaR(layer1, 250, "AEP"))

})

test_that("The layer constructor works", {
  expect_equal(agg_layer$limit, 4000000)
  expect_equal(agg_layer$attachment, 1000000)
  expect_equal(agg_layer$participation, 1)
  expect_equal(agg_layer$agg_attachment, 4000000)
  expect_equal(agg_layer$agg_limit, 12000000)
  expect_equal(agg_layer$loss_set, "yelt_test")
  expect_equal(agg_layer$lobs, c("PHYSICIANS","CHC","MEDCHOICE"))
  ceded1 <- sum(layer2$trial_results$ceded_loss)
  ls <- get(layer2$loss_set)
  x <- subset(ls, LOB == "PHYSICIANS")$Loss
  ceded2 <- sum(pmin(pmax(x - layer2$attachment, 0), layer2$limit))
  expect_equal(ceded1, ceded2)
  expect_equal(nrow(layer2$trial_results), length(unique(ls$trialID)))
})

test_that("The portfolio constructor works", {
  P <- portfolio(layer1, layer2, layer3)
  expect_equal(class(P), "portfolio")
  expect_identical(layer1, P$layer_list[[1]])
  expect_identical(layer2, P$layer_list[[2]])
  expect_identical(layer3, P$layer_list[[3]])
})

test_that("Layer print works", {
  expect_error(print(layer1), NA)
  expect_error(print(test_layer), NA)
  expect_error(print(gross_layer), NA)
  expect_error(print(agg_layer), NA)
  expect_equal(dim(print(agg_layer)), c(7, 1))
  expect_equal(dim(print(test_layer)), c(5, 1))
})

test_that("Portfolio print works", {
  expect_invisible(print(P))
})

test_that("print.layer.summary works", {
  s <- summary(layer1)
  expect_invisible(print(s))
  u <- print(s)
  f <- function(y) format(round(y), big.mark = ",", scientific = FALSE)
  expect_equal(u["Mean:", "Value"], f(expected(layer1)))
  expect_equal(u["StdDev:", "Value"], f(stdev(layer1)))
})


test_that("Portfolio mean is the sum of layer means", {
  layer_sum <- expected(layer1) + expected(layer2) + expected(layer3)
  expect_equal(layer_sum, expected(P))
})

print("test complete")
