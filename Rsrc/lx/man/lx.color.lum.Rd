% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.plot.r
\name{lx.color.lum}
\alias{lx.color.lum}
\title{get luminance (darkness) of color(s)}
\usage{
lx.color.lum(color)
}
\arguments{
\item{color}{vector of any of the three kinds of R color specifications, 
i.e., either a color name (as listed by \link{colors}()),
a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
or a positive integer i meaning \link{palette}()[i]}
}
\value{
numeric vector of luminance
}
\description{
compute luminance of colors for human eye. luminance ranges
between 0 (dark) and 1 (light).
}
\examples{
lx.color.lum(c("black", "white", "red", "yellow"))
}
\seealso{
\link{lx.color.fore}
}
