# -------------------------------------------------
# $Id: lx.osutil.r 396 2019-01-02 22:53:10Z viari $
# lx OS utilities
#

# =================================================
# RScript / R CMD Batch utilities
#

# -------------------------------------------------
#' command line arguments parsing
#' @author Chris Wallace
#' @description
#' get a named list of command line arguments
#'
#' Usage:
#' call the R script as
#' \preformatted{
#'   ./script.R [--]arg[=value]
#' or
#'   R CMD BATCH [--]arg[=value] script.R
#' }
#'
#' Then in R script do
#' \preformatted{
#'   myargs <- lx.getargs()
#' }
#' to retrieve a named list of command line arguments, in the following form:
#' \tabular{llll}{
#' arg_form \tab name \tab value \tab typeof_value\cr
#' ----------- \tab ---- \tab ---- \tab --------\cr
#' [--]arg=val \tab arg \tab val \tab character\cr
#' [--]arg \tab arg \tab TRUE \tab logical\cr
#' }
#' example:
#' \preformatted{
#' myscript.R --a=100 --b=yes c.c
#' }
#' will yields the following named list in R:
#' \preformatted{
#' list(a="100", b="yes", c.c=TRUE)
#' }
#'
#' @param verbose print verbiage to screen 
#' @param defaults a named list of defaults, optional
#' @param .pattern internal pattern used to strip characters in options.
#' default is to strip two leading '-'. A more laxist policy could be "^-+" i.e. 
#' any number of leading '-'.
#' 
#' @return a named list
#' @seealso \link{commandArgs}
#' @note
#' don't confuse this function with \link{lx.args} that handles \bold{function} arguments.
#' @note
#' borrowed from : \href{http://cwcode.wordpress.com/2013/04/16/the-joys-of-rscript/}{http://cwcode.wordpress.com/2013/04/16/the-joys-of-rscript/}
#'
lx.getargs <- function(verbose=FALSE, defaults=NULL, .pattern="^--") {
  myargs <- gsub(.pattern, "", commandArgs(TRUE))
  setopts <- !grepl("=", myargs)
  if (any(setopts))
    myargs[setopts] <- paste0(myargs[setopts], "=notset")
  myargs.list <- strsplit(myargs, "=")
  myargs <- lapply(myargs.list, "[[", 2 )
  names(myargs) <- lapply(myargs.list, "[[", 1)
  
  ## logicals
  if (any(setopts))
    myargs[setopts] <- TRUE
  
  ## defaults
  if (!is.null(defaults)) {
    defs.needed <- setdiff(names(defaults), names(myargs))
    if (length(defs.needed)) {
      myargs[defs.needed] <- defaults[defs.needed]
    }
  }
  
  ## verbage
  if (verbose) {
    cat("read",length(myargs),"named args:\n")
    print(myargs)
  }
  myargs
}

# =================================================
# OS utilities
#

# -------------------------------------------------
#' wrapper to \link{Sys.info}
#' @description get system and user information
#' @param ... 0 or more keys to \link{Sys.info} (if empty, then
#'            return all entries)
#' @return a (possibly empty) vector of entries of \link{Sys.info}
#' @note result may be NULL if \link{Sys.info} is not implemented
#' @examples
#' x <- lx.sysinfo()
#' x <- lx.sysinfo("user", "machine")
#
lx.sysinfo <- function(...) {
  info <- Sys.info()
  if (! is.null(info)) {
    args <- lx.args(-1)
    if (length(args) > 0) {
      args <- unlist(args)
      info <- info[args]
      names(info) <- args
    }
  }
  info
}

# =================================================
# file utilities
#

# -------------------------------------------------
#' filename extension
#' @param path filename
#' @return file (name) extension (excluding the leading dot)
#' @note only purely alphanumeric extensions are recognized
#' @note borrowed from package tools
#' @examples
#' lx.file.ext("dir/file.more.ext")
#
lx.file.ext <- function(path) {
  pos <- regexpr("\\.([[:alnum:]]+)$", path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")
}

# -------------------------------------------------
#' filename without extension
#' @param path filename
#' @return filename without extension (and the leading dot)
#' @note only purely alphanumeric extensions are recognized
#' @note borrowed from package tools
#' @examples
#' lx.file.no.ext("dir/file.more.ext")
#
lx.file.no.ext <- function(path) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", path)
}

# -------------------------------------------------
#' resolve filename
#' @description resolve filename by trying different
#' (combinations of) extensions.
#' @details if \code{path} has the form "sys:package:" then
#' the file is searched in package system files.\cr\cr
#' if \code{ext} is a list of character vectors then
#' extensions from each vector element are catenated 
#' in a 'depth first' way i.e. first element of the first vector
#' with first of the second ... etc (see examples).\cr
#' @param path filename (see details)
#' @param ext a character vector or list of character vectors containing
#' extensions to be combined (see details).
#' @param mode access mode (see \link{file.access})
#' @param .verbose printout filenames trials
#' @param .dedots remove possible double dots in filename
#' (this may occur if you have included "" as an extension (see examples)).
#' @return actual pathname if a file has matched else NULL
#' @note only the first matching filename (if any) is returned.
#' @examples
#' lx.file.resolve("sys:lx:data/lx.iris", c("rds", "RData"))
#' lx.file.resolve("none", .verbose=TRUE)
#' lx.file.resolve("none", c("", "a", "b"), .verbose=TRUE)
#' lx.file.resolve("none", list(c("a", "b"), c("", "c", "d"), c("", "e", "f")), .verbose=TRUE)
#
lx.file.resolve <- function(path, ext="", mode=0,
                            .verbose=FALSE, .dedots=TRUE) {
  
  # internal check file access
  #
  .access <- function(name) {
     ok <- file.access(name, mode=mode) == 0
     if (.verbose) lx.out("trying file: '", name,
                          "' mode: ", mode,
                          " status: ", ok)
     ok
  }
  
  # null case
  if (is.null(path)) return(NULL)
  
  # convert sys:package:xxx filename
  #
  if (grepl("^sys:[^:]+:", path)) {
    splt <- lx.strsplit(path, ":")
    path <- lx.system.file(splt[3], package=splt[2])
  }
  
  # generate extensions
  #
  .ext <- function(x) {
    as.vector(if (length(x) == 0) ""
              else sapply(x[[1]], function(y) paste(y, .ext(x[-1]), sep=".")))
  }
  ext <- unique(gsub("^\\.+|\\.+$", "", 
                     .ext(if (is.list(ext)) ext else list(ext))))

  # check files
  #
  
  for (e in ext) {
    npath <- gsub("\\.+$", "", paste(path, e, sep="."))
    if (.dedots) npath <- gsub("\\.\\.+([^/])", ".\\1", npath)
    if (.access(npath)) return(npath)
  }
  
  # nothing found
  #
  NULL
}
