% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.strings.r
\name{lx.strtrunc}
\alias{lx.strtrunc}
\title{truncate string}
\usage{
lx.strtrunc(s, width, between = "...")
}
\arguments{
\item{s}{string}

\item{width}{maximum width}

\item{between}{between string}
}
\description{
truncate long string as "abcd...wxyz", usually for pretty printing
}
\examples{
lx.strtrunc("the lazy dog", 6)
}
