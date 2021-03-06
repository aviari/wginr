% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.apply.r
\name{lx.rowapply}
\alias{lx.rowapply}
\title{apply function on each row of a data.frame (or matrix).}
\usage{
lx.rowapply(X, FUN, ..., as.list = FALSE, SIMPLIFY = TRUE,
  stringsAsFactors = default.stringsAsFactors(),
  pg.verbose = lx.options("pg.verbose"), use.threads = lx.use.threads(),
  mc.cores = lx.options("mc.cores"), mc.preschedule = TRUE,
  mc.allow.recursive = FALSE)
}
\arguments{
\item{X}{an array}

\item{FUN}{the function to be applied. see \link{lapply}.}

\item{...}{anything passed to FUN. see \link{lapply}.}

\item{as.list}{if TRUE return result as a list of FUN results per row\cr
else returns a data.frame or a vector (see below)}

\item{SIMPLIFY}{if result has only one column convert it to vector}

\item{stringsAsFactors}{should character columns be converted to factors}

\item{pg.verbose}{use progress bar}

\item{use.threads}{use multithreading}

\item{mc.cores}{The number of cores to use. see \link{mclapply}.}

\item{mc.preschedule}{perform prescheduling. see \link{mclapply}.}

\item{mc.allow.recursive}{allow recursive call. see \link{mclapply}.}
}
\value{
if \code{as.list==TRUE} returns a list else returns 
a data.frame or a vector if there is only one column and SIMPLIFY==TRUE.
}
\description{
\link{apply} sucks because it coerces its input to an array
before applying function. In case of a data.frame this will
coerce all data to the same type (usually character).\cr
this function performs as \code{apply(X, 1, FUN, ...)}
but passing a correctly typed (and named) row to FUN.\cr
In addition it can be multithreaded as \link{lx.lapply}.\cr
Note that there is no \code{'lx.colapply'} equivalent 
since \code{lapply} (and \code{lx.lapply}) will usually 
do the (per column) job on a dataframe.
}
\note{
when \code{as.list==FALSE} (default) all results from \code{FUN}
should be of the same size to be properly rehaped to a data.frame
}
\examples{
data(iris)
x <- lx.rowapply(iris, function(x) x$Sepal.Width * x$Sepal.Length)
identical(x, iris$Sepal.Width * iris$Sepal.Length)
x <- lx.rowapply(iris, function(x) list(area=x$Sepal.Width * x$Sepal.Length, 
                                        name=x$Species))

}
