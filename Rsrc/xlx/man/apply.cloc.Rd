% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.apply.r
\name{apply.cloc}
\alias{apply.cloc}
\title{apply function to clocations by chromosomes}
\usage{
apply.cloc(clocations, fun, ..., handle = NULL, keep.order = TRUE,
  use.threads = lx.use.threads(), mc.cores = lx.options(mc.cores))
}
\arguments{
\item{clocations}{nx3 matrix of clocations}

\item{fun}{: function or function name called as \code{fun(cloc, handle, ...)}}

\item{...}{anything passed to \code{fun}}

\item{handle}{optional (basta or baf) file handle. if use.threads==TRUE 
then handle will be properly duplicated thru calls (as with \link{lx.happly})}

\item{keep.order}{keep \code{fun} results in the same order as clocations}

\item{use.threads}{(see \link{lx.use.threads})}

\item{mc.cores}{number of processes (see \link{HELP.LX.OPTIONS})}
}
\value{
a (unnamed) vector of results of \code{fun}
}
\description{
this is a variant of \link{apply.clocs} where \code{fun} is
called on each clocation (instead of matrix of clocations).
it can be viewed as a simple \code{apply(clocations, 1, fun, ...)}
except that the job is actually split by chromosome for multithreading.\cr
}
\details{
\code{fun} first argument is a single clocation
\bold{not} a matrix of clocations as in \link{apply.clocs}
}
\note{
if result order is not important, then use keep.order=FALSE (this will slightly
speedup the operation and save memory)
}
\examples{
fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
slocs <- c("seq1:1-10", "seq2:2-3", "seq1:15-20") 
clocs <- clocations(lapply(slocs, sloc2cloc, handle=fh))
seqs <- apply.cloc(clocs, function(cloc, handle) {
                       basta.fetch.cloc(handle, cloc)
                     }, handle=fh, use.threads=TRUE, mc.cores=2)
seqs <- apply.cloc(clocs, function(cloc, handle, foo) {
                paste0(foo, basta.fetch.cloc(handle, cloc))
}, handle=fh, foo="seq:", use.threads=TRUE, mc.cores=2)

basta.close(fh)

}
\seealso{
\link{apply.clocs}
}
