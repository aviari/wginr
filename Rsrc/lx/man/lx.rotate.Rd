% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.language.r
\name{lx.rotate}
\alias{lx.rotate}
\title{rotate vector circularly n times}
\usage{
lx.rotate(x, n = 1L)
}
\arguments{
\item{x}{vector}

\item{n}{number of rotations.\cr 
if > 0 rotate to left else rotate to right}
}
\value{
rotated vector
}
\description{
rotate vector circularly n times
}
\examples{
lx.rotate(1:5, 2)  # => 3 4 5 1 2
lx.rotate(1:5, -2) # => 4 5 1 2 3
}
