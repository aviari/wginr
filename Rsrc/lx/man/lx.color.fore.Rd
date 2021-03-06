% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.plot.r
\name{lx.color.fore}
\alias{lx.color.fore}
\title{choose foreground color (black or white) from background color}
\usage{
lx.color.fore(color, threshold = 0.5, white = "white", black = "black")
}
\arguments{
\item{color}{vector of any of the three kinds of R color specifications, 
i.e., either a color name (as listed by \link{colors}()),
a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
or a positive integer i meaning \link{palette}()[i]}

\item{threshold}{luminance threshold}

\item{white}{foreground color for dark background}

\item{black}{foreground color for light background}
}
\value{
numeric vector of foreground colors
}
\description{
compute the best foreground color for given
background colors according to background luminance
}
\examples{
lx.color.fore(c("black", "white", "red", "yellow"))
}
\seealso{
\link{lx.color.lum}
}
