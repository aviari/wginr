% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.apply.r
\name{lx.use.threads}
\alias{lx.use.threads}
\title{tell if lx can use multithreading}
\usage{
lx.use.threads()
}
\description{
multithreading is on when
lx.options(use.threads) is TRUE
and lx.options(mc.cores) > 1
}
\note{
threads are implemented in the \code{parallel}
library by using \code{fork}, and will
run parallel processes even if lx.options(mc.cores)
is greater than the actual number of cores. In this case
the overall process may slow donw considerably.
}
