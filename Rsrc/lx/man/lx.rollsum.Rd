% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.language.r
\name{lx.rollsum}
\alias{lx.rollsum}
\title{rolled sum}
\usage{
lx.rollsum(x, k, drop = FALSE)
}
\arguments{
\item{x}{vector}

\item{k}{number of consecutive elements to sum (k >= 1)}

\item{drop}{drop the last (k-1) elements of result}
}
\value{
vector of size length(x) if (drop==F) or length(x)-k+1
if (drop==F), containing the sum of k consecutive elements.
}
\description{
compute the rolling sum of k consecutive elements in vector.
this is equivalent to
\code{sapply(seq_along(x), function(i) sum(x[i:(i+(k-1))], na.rm=T))}
but is much quicker since it runs in \code{O(n)}.
}
\note{
with drop==F the last (k-1) elements will
sum up less than k elements.
}
\examples{
lx.rollsum(1:5, 2)          # => 3 5 7 9 5
lx.rollsum(rep(1,10), 3, TRUE) # => 3 3 3 3 3 3 3 3
}
\seealso{
\link{lx.binsum} for a jumping (instead of rolling) version
}
