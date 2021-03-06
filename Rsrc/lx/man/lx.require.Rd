% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.packages.r
\name{lx.require}
\alias{lx.require}
\title{check and optionally install package}
\usage{
lx.require(package, install = TRUE, load = FALSE, tarball = NULL,
  silent = FALSE, ...)
}
\arguments{
\item{package}{package name (as string or symbol)}

\item{install}{perform installation if not installed}

\item{load}{additionnaly load package}

\item{tarball}{local tarball where to find package
(in that case you should also specify \bold{repos=NULL} in
\code{...} parameters}

\item{silent}{do not print any message (see notes)}

\item{...}{any parameter of \link{install.packages}}
}
\value{
invisible(TRUE) if package is (now) available
else invisible(FALSE)
}
\description{
check if package is present and install it if
necessary
}
\details{
first check if package is present in \link{installed.packages}.
if not and if \code{install} is TRUE then install it using
\link{install.packages} with provided arguments.
}
\note{
\code{silent} will not silent the (possible) installation phase
      because its a bad idea to download and install anything without
      notification. to force complete silence see \link{install.packages}
      options (quietly and verbose).

you may silently check the existence of a package by
\code{lx.require(package, install=FALSE, load=FALSE, silent=TRUE)}
}
