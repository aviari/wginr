# -------------------------------------------------
# $Id: lx.packages.r 396 2019-01-02 22:53:10Z viari $
# lx packages utilities
#

# -------------------------------------------------
#' find names of R system files
#' @description
#' this is nearly identical to \code{system.file}
#' but does not check at all for file existence.
#'
#' @param filename filename to append to path
#' @param package package name
#' @return <packageRootDir>/[filename]
#'

lx.system.file <- function(filename="", package="lx") {
 filename <- gsub("^/+", "", filename) # remove leading '/'
 file.path(path.package(package=package, quiet=TRUE), filename)
}

# -------------------------------------------------
#' check and optionally install package
#' @description
#' check if package is present and install it if
#' necessary
#' 
#' @param package package name (as string or symbol)
#' @param install perform installation if not installed
#' @param load additionnaly load package
#' @param tarball local tarball where to find package
#'        (in that case you should also specify \bold{repos=NULL} in
#'        \code{...} parameters
#' @param silent do not print any message (see notes)
#' @param ... any parameter of \link{install.packages}
#' @return invisible(TRUE) if package is (now) available
#' else invisible(FALSE)
#'
#' @details
#' first check if package is present in \link{installed.packages}.
#' if not and if \code{install} is TRUE then install it using
#' \link{install.packages} with provided arguments.
#'
#' @note \code{silent} will not silent the (possible) installation phase
#'       because its a bad idea to download and install anything without
#'       notification. to force complete silence see \link{install.packages}
#'       options (quietly and verbose).
#'       
#' @note you may silently check the existence of a package by
#' \code{lx.require(package, install=FALSE, load=FALSE, silent=TRUE)}
#
lx.require <- function(package, install=TRUE, load=FALSE, tarball=NULL, silent=FALSE, ...) {
  .silent <- if (silent) suppressMessages else identity
  .installed <- function(package) {
    package %in% rownames(installed.packages())
  }
  .load <- function(package) {
    if (load) .silent(do.call(require, list(package)))
    else TRUE
  }
  .out <- function(...) {
    if (! silent) cat(paste0("+ ", ..., "\n"), file=stderr())
  }

  package <- as.character(lx.args(2))

  if (.installed(package))
    return(invisible(.load(package)))

  .out("This package requires package '", package,
        "' that is not currently installed.")

  packarg <- if (is.null(tarball)) package else tarball
  
  cmd <- deparse(substitute(install.packages(packarg, ...)))

  if (install) {
    .out("Now trying to install package using:")
    .out("   ", cmd)
    do.call(install.packages, c(packarg, list(...)), envir=.GlobalEnv)
  }
  else {
    .out("Please install package using:")
    .out("   ", cmd)
  }

  return(invisible(.installed(package)))
}
