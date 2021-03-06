% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.plot.r
\name{lx.color.alpha}
\alias{lx.color.alpha}
\title{add alpha channel to color(s)}
\usage{
lx.color.alpha(color, alpha = 1)
}
\arguments{
\item{color}{vector of any of the three kinds of R color specifications, 
i.e., either a color name (as listed by \link{colors}()),
a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
or a positive integer i meaning \link{palette}()[i]}

\item{alpha}{alpha channel value (0 <= alpha <= 1)\cr
alpha=0: completely transparent, alpha=1: completely opaque}
}
\value{
modified colors (as hexadecimal strings)
}
\description{
add alpha channel to color(s)
}
\examples{
lx.color.alpha("red", 0)  # => #ff000000
lx.color.alpha("red", 1)  # => #ff0000ff
}
\seealso{
\link{lx.color.change}, \link{lx.color.light}
}
