% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.table.r
\name{lx.table.bysets}
\alias{lx.table.bysets}
\title{cross table of sets intersections}
\usage{
lx.table.bysets(lsets)
}
\arguments{
\item{lsets}{a list of n named sets Si}
}
\value{
n x n table where entry T[i,j] equals the size of Si inter Sj
}
\description{
cross table of sets intersections
}
\note{
aka \code{outer(lsets, lsets, function(x,y) \{ length(intersect(x,y)) \})}
      but \code{outer} does not work for this
}
\examples{
lx.table.bysets(list(a=c(1,2), b=c(1,3), c=c(2,3,4)))
# this the same as 
lx.table.bysets(list(a=c(1,2,2), b=c(1,3), c=c(2,3,4,3)))
# since a, b, c are considered to as sets
}
\seealso{
\link{lx.tables}
}
