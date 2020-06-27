
<!-- README.md is generated from README.Rmd. Please edit that file -->

# layers

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/jfkingiii/layers.svg?branch=master)](https://travis-ci.com/jfkingiii/layers)
<!-- badges: end -->

The goal of layers is to provide a simple library for doing common
reinsurance calculations outside of Analyze Re.

## Installation

You can install the development version of `layers` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jfkingiii/layers")
```

## Example

Use the included sample yelt, `yelt_test` to try out some of the
`layers` functions.

``` r
library(layers)

# Look at the help files
help(package = layers)

# Create some layers and get their metrics
options(scipen = 999)
test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE","HOSPITAL"))
# Aggregate limits and deductibles are optional and default to UNLIMITED and 0.
test_layer
#> Limit:        4,000,000 
#> Attachment:   1,000,000 
#> Participation:    1.000 
#> Loss set:     yelt_test 
#> LOBs:         PHYSICIANS CHC MEDCHOICE

gross_layer
#> Limit:        UNLIMITED 
#> Attachment:   0 
#> Participation:    1.000 
#> Loss set:     yelt_test 
#> LOBs:         PHYSICIANS CHC MEDCHOICE HOSPITAL

summary(test_layer)
#> Limit:        4,000,000 
#> Attachment:   1,000,000 
#> Participation:    1.000 
#> Loss set:     yelt_test 
#> LOBs:         PHYSICIANS CHC MEDCHOICE 
#> 
#>                Value
#> Mean:      6,782,625
#> StdDev:    3,381,071
#> VaR 25:   13,889,280
#> VaR 100:  16,049,317
#> VaR 250:  18,392,032
#> tVaR 25:  15,582,691
#> tVaR 100: 17,798,589
#> tVaR 250: 19,018,259

summary(gross_layer)
#> Limit:        UNLIMITED 
#> Attachment:   0 
#> Participation:    1.000 
#> Loss set:     yelt_test 
#> LOBs:         PHYSICIANS CHC MEDCHOICE HOSPITAL 
#> 
#>                 Value
#> Mean:     102,235,225
#> StdDev:    16,176,233
#> VaR 25:   134,029,327
#> VaR 100:  148,713,927
#> VaR 250:  152,354,245
#> tVaR 25:  141,894,334
#> tVaR 100: 151,802,890
#> tVaR 250: 154,059,363

# Test some OEPs
VaR(gross_layer, 5, "OEP")
#> [1] 11000000
VaR(gross_layer, 10, "OEP")
#> [1] 15000000
VaR(test_layer, 15, "OEP")
#> [1] 4000000
VaR(test_layer, 20, "OEP")
#> [1] 4000000

# Try a layer with aggregate parameters
agg_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"),
                   agg_attachment = 4000000, agg_limit = 12000000)
summary(agg_layer)
#> Limit:        4,000,000 
#> Attachment:   1,000,000 
#> Participation:    1.000 
#> Agg Attachment:   4,000,000 
#> Agg Limit:    12,000,000 
#> Loss set:     yelt_test 
#> LOBs:         PHYSICIANS CHC MEDCHOICE 
#> 
#>                Value
#> Mean:      3,037,403
#> StdDev:    2,995,537
#> VaR 25:    9,889,280
#> VaR 100:  12,000,000
#> VaR 250:  12,000,000
#> tVaR 25:  11,133,044
#> tVaR 100: 12,000,000
#> tVaR 250: 12,000,000

# Portolio expected losses
layer1 <- layer(1000000, 4000000, 1, "yelt_test", lobs="PHYSICIANS")
layer2 <- layer(5000000, 5000000, 1, "yelt_test", lobs="PHYSICIANS")
layer3 <- layer(1000000, 10000000, 1, "yelt_test", lobs="PHYSICIANS")
P <- portfolio(layer1, layer2, layer3)

expected(layer1) + expected(layer2) + expected(layer3)
#> [1] 258788
expected(P)
#> [1] 258788
```
