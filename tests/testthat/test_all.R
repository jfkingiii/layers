library(layers)

test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE","HOSPITAL"))
agg_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"),
                   agg_attachment = 4000000, agg_limit = 12000000)


test_that("expected is accurate", {
  expect_equal(trunc(expected(test_layer)), 6782624)
  expect_equal(trunc(expected(gross_layer)), 102235224)
  expect_equal(trunc(expected(agg_layer)), 3037403)
})
