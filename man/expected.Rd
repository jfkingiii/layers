% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R, R/layer.R, R/portfolio.R
\name{expected}
\alias{expected}
\alias{expected.layer}
\alias{expected.portfolio}
\title{Compute the expected losses ceded to the layer or portfolio.}
\usage{
expected(object)

\method{expected}{layer}(object)

\method{expected}{portfolio}(object)
}
\arguments{
\item{object}{the layer or portfolio to compute the expectation of}
}
\description{
Compute the expected losses ceded to the layer or portfolio.
}
\examples{
test_layer <- layer(4000000, 1000000, 1, "yelt_test", lobs=c("PHYSICIANS","CHC","MEDCHOICE"))
expected(test_layer)
}
