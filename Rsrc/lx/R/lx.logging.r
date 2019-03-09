# -------------------------------------------------
# $Id: lx.logging.r 396 2019-01-02 22:53:10Z viari $
# lx logging utilities
#

# -------------------------------------------------
# <internal> <no_export>
# get verbose level from num or string
#
.verbose.level <- function(level) {
  level <- level[[1]]
  if (is.numeric(level))
    level
  else {
    level <- match.arg(as.character(level), c("debug", "info", "warning", "error"))
    switch (level,
      debug   = 0L,
      info    = 10L,
      warning = 20L,
      error   = 100L
    )
  }
}

# -------------------------------------------------
#' set/get verbose level
#' @description 
#' \bold{set mode:} \code{lx.verbose(level)}
#' where \code{level} is either an integer value or one of
#' "debug" (=0); "info" (=10); "warning" (=20); "error" (=100)\cr
#' \bold{get mode:} \code{lx.verbose()}
#' @param level verbose level (see description)
#' @return in get mode: return current verbose level\cr
#' in set mode return previous verbose level (i.e. before set)
#' @examples
#' p <- lx.verbose("debug") # set to debug and return previous
#' lx.verbose(p)            # restore to previous value
#
lx.verbose <- function(level) {
  res <- if (! missing(level)) {
            prev <- lx.options("verbose")
            lx.options(verbose=.verbose.level(level))
            prev
        } else 
            lx.options("verbose")
  .verbose.level(res)
}

# -------------------------------------------------
#' printout information on stderr
#' @description
#' prints arguments on stderr if \code{level >= lx.verbose()}
#' @param ... zero or more objects which can be coerced to character 
#' (and which are pasted together with no separator).
#' @param level verbose level.
#' @param with.mem if TRUE, additionaly printout memory usage
#' @param with.caller if TRUE, additionaly printout caller
#' @param up.frame if with.caller==TRUE, caller position in call stack (see details).
#' @details
#' if \code{with.mem} = TRUE this fonction will calls \code{gc()}, and
#' may therefore slow down process.\cr
#'
#' the \code{up.frame} parameter indicates the number of frames to go up to
#' get caller. default (up=0) means to retrieve arguments from the function
#' calling \code{lx.out}; up=1 means the function calling the function
#' calling \code{lx.out} and so on up to top level.\cr
#'
#' the \code{up.frame} parameter is useful if you called \code{lx.out} from
#' an error reporting function and want to report the caller function
#' (see examples below)
#'
#' @seealso \link{lx.verbose} \link{lx.warn}, \link{lx.warnif}, \link{lx.stopif}
#' 
#' @examples
#' lx.out("pi=", pi, " and e=", exp(1))
#' lx.out("pi=", pi, " and e=", exp(1), with.mem=TRUE)
#'
#' my_report <- function(...) lx.out("my report: ", ..., up.frame=1)
#' foo <- function() { x <- 1; my_report("x=", x) }
#' foo()
#
lx.out <- function(..., level="info", with.mem=FALSE, with.caller=TRUE, up.frame=0L) {
  verbose <- .verbose.level(level) >= lx.verbose()
  if (verbose) {
    mem <- if (with.mem) {
             paste0("[", format(sum(gc()[,2]), nsmall=1), " Mb] ")
           } else {""}
    cal <- if (with.caller) {
             caller <-lx.args(1, up.frame=up.frame+1)[[1]]
             caller <- if (is.null(caller)) "top-level" 
                           else if (typeof(caller) == "closure") "closure"
                           else paste(as.character(caller), collapse=":")
             paste0("[", caller, "] ", collapse="")
           } else {""}
    args <- paste0(as.character(list(...)), collapse="")
    cat(paste0("+ [", date(), "]\t", cal, args, " ", mem, "\n"), file=stderr())
    flush(stderr())
  }
  invisible(verbose)
}

# -------------------------------------------------
#' emit warning
#' @description
#' prints warning on stderr
#' @param ... anything accepted by \link{lx.out}
#' @param level see \link{lx.out}
#' @param up.frame see \link{lx.out}
#' @seealso \link{lx.out}, \link{lx.warnif}, \link{lx.stopif}
#' @examples
#' lx.warn("pi=", pi, with.mem=TRUE)
#
lx.warn <- function (..., level="warning", up.frame=0L) {
  lx.out(" * Warning * ", ..., level=level, up.frame=up.frame+1)
}

# -------------------------------------------------
#' emit warning if condition is true
#' @param condition boolean value
#' @param ... anything accepted by \link{lx.out}
#' @param level see \link{lx.out}
#' @param up.frame see \link{lx.out}
#' @return invisible(condition)
#' @seealso \link{lx.out}, \link{lx.warn}, \link{lx.stopif}
#' @examples
#' x <- -1
#' lx.warnif(x < 0, "x=", x, " is negative")
#
lx.warnif <- function (condition, ..., level="warning", up.frame=0L) {
  if (condition)
    lx.warn(..., level=level, up.frame=up.frame+1)
  invisible(condition)
}

# -------------------------------------------------
#' print call traceback
#' @description
#' print call traceback on stderr
#' @param level see \link{lx.out}
#' @param up.frame see \link{lx.out}
#' @return invisible(traceback list from \link{traceback})
#'
lx.traceback <- function(level="debug", up.frame=0L) {
  trac <- sapply(sys.calls(), deparse)
  trac <- trac[seq_len(max(0, length(trac)-up.frame-1))]
  lx.out("* Traceback *", level=level, with.caller=FALSE)
  sapply(trac, function(x, ...) lx.out("[trace] ", x, ...),
               level=level, with.caller=FALSE)
  invisible(trac)
}

# -------------------------------------------------
#' stop execution if condition is true
#' @param condition boolean value
#' @param ... anything accepted by \link{lx.out}
#' @param level see \link{lx.out}
#' @param up.frame see \link{lx.out}
#' @param trace printout traceback
#' @return invisible(condition)
#' @seealso \link{lx.out}, \link{lx.warn}, \link{lx.warnif}
#' @examples
#' \dontrun{
#' x <- -1
#' lx.stopif(x < 0, "x=", x, " is negative")
#' }
#'
lx.stopif <- function (condition, ..., level="error", up.frame=0L, trace=TRUE) {
  if (condition) {
    lx.out("* Error * ", ..., level=level, up.frame=up.frame+1)
    if (trace)
      lx.traceback(level=level, up.frame=up.frame+1)
    stop(" * Aborted *", call. = FALSE)
  }
  invisible(condition)
}
