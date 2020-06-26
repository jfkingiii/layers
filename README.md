
<!-- README.md is generated from README.Rmd. Please edit that file -->

# layers

<!-- badges: start -->

<!-- badges: end -->

The goal of layers is to provide a simple library for doing common
reinsurance calculations outside of Analyze Re. \#\# Installation

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
help(package=layers)

# Create some layers and get their metrics
options(scipen = 999)
test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
gross_layer <- layer(UNLIMITED, 0, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE","HOSPITAL"))
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
#> VaR 25:   13,797,776
#> VaR 100:  15,897,134
#> VaR 250:  18,093,981
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
#> VaR 25:   133,856,576
#> VaR 100:  146,607,414
#> VaR 250:  152,166,934
#> tVaR 25:  141,894,334
#> tVaR 100: 151,802,890
#> tVaR 250: 154,059,363

# Test some OEPs
VaR(gross_layer, 1 - 1/25, "OEP")
#> [1] 1321227
VaR(gross_layer, 1 - 1/100, "OEP")
#> [1] 3000000
VaR(test_layer, 1 - 1/100, "OEP")
#> [1] 2000000
VaR(test_layer, 1 - 1/100, "OEP")
#> [1] 2000000
```
