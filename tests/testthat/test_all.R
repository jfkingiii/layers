library(layers)

print("running test")

test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE","HOSPITAL"))
agg_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"),
                   agg_attachment = 4000000, agg_limit = 12000000)
layer1 <- layer(1000000, 4000000, 1, "yelt_test", lobs="PHYSICIANS")
layer2 <- layer(5000000, 5000000, 1, "yelt_test", lobs="PHYSICIANS")
layer3 <- layer(1000000, 10000000, 1, "yelt_test", lobs="PHYSICIANS")

test_that("expected is accurate", {
  expect_equal(trunc(expected(test_layer)), 678262)
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


test_that("Portfolio mean is the sum of layer means", {
  layer_sum <- expected(layer1) + expected(layer2) + expected(layer3)
  P <- portfolio(layer1, layer2, layer3)
  expect_equal(layer_sum, expected(P))
})

print("test complete")
