% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.plot.r
\name{lx.rainbow}
\alias{lx.rainbow}
\title{rainbow color scale}
\usage{
lx.rainbow(n, skip = 1/6, ...)
}
\arguments{
\item{n}{the number of required colors}

\item{skip}{end portion of color circle to remove (default=1/6)}

\item{...}{any argument of \link{rainbow}}
}
\value{
a character vector of colors
}
\description{
similar to \link{rainbow} except that colors do not close
circularly (i.e. start and end colors are not the same).
}
\examples{
lx.rainbow(10)
}
