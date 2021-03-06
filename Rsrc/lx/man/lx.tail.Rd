% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.language.r
\name{lx.tail}
\alias{lx.tail}
\title{tail of vector}
\usage{
lx.tail(x, n = 1L)
}
\arguments{
\item{x}{vector}

\item{n}{if positive take n last elements, if negative take all but n first elements}
}
\value{
vector of size n if n >= 0, or of size \code{length(x) - n} if n < 0
}
\description{
same as \link{tail} but limited to vectors and quicker
}
\examples{
lx.tail(1:10, 2)  # => 9 10
lx.tail(1:10, -2) # => 3 4 5 6 7 8 9 10
}
\seealso{
\link{tail} \link{lx.head}
}
