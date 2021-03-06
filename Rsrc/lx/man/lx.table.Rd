% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.table.r
\name{lx.table}
\alias{lx.table}
\title{count or contingency table with specified levels}
\usage{
lx.table(..., levels = NULL)
}
\arguments{
\item{...}{same as \link{table}}

\item{levels}{levels to force}
}
\description{
same as \link{table} but force levels.
(call \link{factor} on \code{...} then call \link{table})
}
\note{
\link{table} is equivalent to \code{lx.table(...)}
}
\examples{
x <- round(runif(10, 1, 10))
lx.table(x, levels=1:10)
y <- round(runif(10, 1, 10))
lx.table(x, y, levels=1:10)

}
\seealso{
\link{lx.tables}
}
