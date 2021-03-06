% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.table.r
\name{lx.condsum}
\alias{lx.condsum}
\title{tabulate conditional sum of integer values.}
\usage{
lx.condsum(xvar, yvar, zero.rm = TRUE, .maxrange = 1e+09, .maxrate = 1)
}
\arguments{
\item{xvar, yvar}{integer vectors of the same length}

\item{zero.rm}{should zero counts be removed from results (as in \link{aggregate})}

\item{.maxrange, .maxrate}{internal parameters (see details)}
}
\value{
an nx3 numeric matrix with columns namdes \code{x, cnt and sy}.\cr
\code{x}   : tabulated values of x\cr
\code{cnt} : counts for each x\cr
\code{sy}  : sum(y|x) the sum of y values for each x
}
\description{
let xvar and yvar two integer vectors, this function
tabulates the conditional \code{sum(yvar|xvar)}. This is
similar to \link{aggregate}\code{(y,x,FUN=sum)} but much quicker
and limited to integer variables.
}
\details{
the function uses two different algorithms (called rle and crossprod)
depending upon the length and range of xvar and yvar. crossprod is
usually quicker when x-range and y-range are small and n is large 
but uses more memory. rle runs in \code{O(n.log(n))} and depends only on
x-range.\cr
the choice is done on the following (simplified) condition\cr
\code{if ((xrange*yrange > .maxrange) || (xrange*yrange/n.log(n) > .maxrate))
then rle else crossprod}. this is a tradeoff between memory usage and speed.\cr
if you want to experiment with both algorithms to fit your particular
data, you may therefore force rle by setting \code{.maxrange=0} and force
crossprod by setting \code{.maxrange=Inf and .maxrate=Inf}
}
\examples{
n = 1e4
x <- floor(runif(n=n, min=10, max=1000))
y <- rpois(n=n, lambda=x)
ax <- aggregate(y, by=list(x), FUN=sum)
bx <- lx.condsum(x, y)
cx <- lx.condsum(x, y, .maxrange=0) # force rle
all.equal(as.matrix(ax), bx[,c(1,3)], check.attributes=FALSE)
identical(bx, cx)

y <- y[x!=500]
x <- x[x!=500]
bx <- lx.condsum(x, y, zero.rm=FALSE)
cx <- lx.condsum(x, y, zero.rm=FALSE, .maxrange=0)
identical(bx, cx)

}
