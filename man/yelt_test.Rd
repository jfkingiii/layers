% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\docType{data}
\name{yelt_test}
\alias{yelt_test}
\title{A sample YELT (year event loss table)}
\format{
A data frame with 127648 rows and 5 variables:
\describe{
\item{\code{trialID}}{Identifier for the trial, which represents one year}
\item{\code{LOB}}{Line of business or other segmentation}
\item{\code{Loss}}{Loss amount}
\item{\code{Sequence}}{Sequence of loss within trial}
}
}
\usage{
yelt_test
}
\description{
A sample YELT (year event loss table) to test layers functions on.
The YELT dataframe must have \code{trial_count} and \code{lobs} attributes to work with the
layers functions.
}
\section{Attributes}{
\describe{
\item{\code{num_trials}}{The number of trials}
\item{\code{lobs}}{The lines of business}
}
}

\examples{
aggregate(yelt_test["Loss"], yelt_test["LOB"], sum)
attributes(yelt_test)
attributes(yelt_test)[c("lobs", "trial_count")]
}
\keyword{datasets}
