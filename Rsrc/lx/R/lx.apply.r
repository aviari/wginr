# -------------------------------------------------
# $Id: lx.apply.r 321 2017-08-18 11:10:19Z viari $
#
# lx apply functions
#

# =================================================
# progress bar (PG)
# based on Mark Heckman post : 
# http://ryouready.wordpress.com/2010/01/11/progress-bars-in-r-part-ii-a-wrapper-for-apply-functions/
#

# -------------------------------------------------------
# <internal> PG generic function for [s|l]apply
#
.lx.pg.meta <- function(METAFUN, X, MARGIN=NULL, FUN, ...) {
  env <- environment()
  total <- if (is.null(MARGIN)) length(X) else sum(dim(X)[MARGIN])
  counter <- 0
  pb <- txtProgressBar(min = 0, max = total, style = lx.options("pg.options.style"))
  
  wrapper <- function(...) {
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1 ,envir= env)
    setTxtProgressBar(get("pb", envir= env), curVal +1)
    FUN(...)
  }
  
  tim <- system.time(res <- if (is.null(MARGIN)) METAFUN(X, wrapper, ...)
                            else                 METAFUN(X, MARGIN, wrapper, ...))
  
  if (lx.options("pg.options.time")) print(tim)
  
  close(pb)
  res
}

# =================================================
# multicore functions
#

# ---------------------------------------------
#' tell if lx can use multithreading
#' @description
#' multithreading is on when
#' lx.options(use.threads) is TRUE
#' and lx.options(mc.cores) > 1
#' @note
#' threads are implemented in the \code{parallel}
#' library by using \code{fork}, and will
#' run parallel processes even if lx.options(mc.cores)
#' is greater than the actual number of cores. In this case
#' the overall process may slow donw considerably.
#'
lx.use.threads <- function() {
  lx.options("use.threads") && (lx.options("mc.cores") > 1)
}

# =================================================
# apply wrappers
#

# ---------------------------------------------
#' apply wrapper
#' @description
#' \code{lx.apply} will call different \link{apply} flavors
#' depending upon the \code{pg.verbose}.argument:\cr
#' if pg.verbose is TRUE then\cr
#' __use \link{apply} with progress bar\cr
#' else\cr
#' __use \link{apply} without progress bar\cr
#'
#' @param X an array, including a matrix. see \link{apply}.
#' @param MARGIN a vector giving the subscripts which the
#' function will be applied over. see \link{apply}.
#' @param FUN the function to be applied. see \link{apply}.
#' @param ... anything passed to FUN. see \link{apply}.
#' @param pg.verbose use progress bar
#' @note
#' default value for  \code{pg.verbose} is taken from \link{lx.options}
#' and may therefore be controlled globally.
#' @note
#' there is no multithread flavor yet, see \link{lx.rowapply}, \link{lx.lapply}
#' @examples
#' n <- 100
#' lx.options(pg.verbose=TRUE)
#' system.time(x <- lx.apply(matrix(1:n, ncol=2), 1, function(x) sum(rnorm(5000*sum(x)))))
#' lx.options(pg.verbose=FALSE)
#' system.time(x <- lx.apply(matrix(1:n, ncol=2), 1, function(x) sum(rnorm(5000*sum(x)))))
#'
lx.apply <- function(X, MARGIN, FUN, ...,
                        pg.verbose=lx.options("pg.verbose")) {
  if (pg.verbose) {
    .lx.pg.meta(apply, X, MARGIN, FUN, ...)
  }
  else {
    apply(X, MARGIN, FUN, ...)
  }
}

# ---------------------------------------------
#' lapply wrapper
#' @description
#' \code{lx.lapply} will call different \link{lapply} flavors
#' depending upon the \code{use.threads} and \code{pg.verbose}.
#' arguments:\cr
#' if use.threads is TRUE then\cr
#' __use \link{mclapply}\cr
#' else if pg.verbose is TRUE then\cr
#' __use \link{lapply} with progress bar\cr
#' else\cr
#' __use \link{lapply} without progress bar\cr
#' @param X an array, including a matrix. see \link{lapply}.
#' @param FUN the function to be applied. see \link{lapply}.
#' @param ... anything passed to FUN. see \link{lapply}.
#' @param pg.verbose use progress bar
#' @param use.threads use multithreading
#' @param mc.cores The number of cores to use. see \link{mclapply}.
#' @param mc.preschedule perform prescheduling. see \link{mclapply}.
#' @param mc.allow.recursive allow recursive call. see \link{mclapply}.
#' @note
#' default value for \code{use.threads} and \code{pg.verbose} are 
#' taken from \link{lx.options}
#' and may therefore be controlled globally.
#' @seealso \link{lx.happly} for a version dulpicating file handle.
#'
#' @examples
#' n <- 100
#' lx.options(pg.verbose=TRUE)
#' system.time(x <- lx.lapply(1:n, function(x) rnorm(5000*x)))
#' lx.options(pg.verbose=FALSE)
#' system.time(x <- lx.lapply(1:n, function(x) rnorm(5000*x)))
#'
lx.lapply <- function(X, FUN, ..., pg.verbose=lx.options("pg.verbose"),
                                   use.threads=lx.use.threads(),
                                   mc.cores=lx.options("mc.cores"),
                                   mc.preschedule=TRUE,
                                   mc.allow.recursive=FALSE) {
  if (use.threads) {
    parallel::mclapply(X, FUN, ..., mc.cores=mc.cores,
                                    mc.preschedule=mc.preschedule,
                                    mc.allow.recursive=mc.allow.recursive)
  }
  else if (pg.verbose) {
    .lx.pg.meta(lapply, X, MARGIN=NULL, FUN, ...)
  }
  else {
    lapply(X, FUN, ...)
  }
}

# ---------------------------------------------
#' sapply wrapper
#' @description
#' \code{lx.sapply} will call different \link{sapply} flavors
#' depending upon the \code{pg.verbose}.argument:\cr
#' if pg.verbose is TRUE then\cr
#' __use \link{sapply} with progress bar\cr
#' else\cr
#' __use \link{sapply} without progress bar\cr
#' @inheritParams lx.lapply
#' @note
#' default value for  \code{pg.verbose} is taken from \link{lx.options}
#' and may therefore be controlled globally.
#' @note
#' there is no multithread flavor yet, see \link{lx.lapply}
#'
#' @examples
#' n <- 100
#' lx.options(pg.verbose=TRUE)
#' system.time(x <- lx.sapply(1:n, function(x) rnorm(5000*x)))
#' lx.options(pg.verbose=FALSE)
#' system.time(x <- lx.sapply(1:n, function(x) rnorm(5000*x)))
#'
lx.sapply <- function(X, FUN, ..., pg.verbose=lx.options("pg.verbose")) {
  if (pg.verbose) {
    .lx.pg.meta(sapply, X, MARGIN=NULL, FUN, ...)
  }
  else {
    sapply(X, FUN, ...)
  }
}

# ---------------------------------------------
#' mapply wrapper
#' @description
#' \code{lx.mapply} will call different \link{mapply} flavors
#' depending upon the \code{use.threads}.argument:\cr
#' if use.threads is TRUE then\cr
#' __use \link{mcmapply}\cr
#' else\cr
#' __use \link{mapply} (without progress bar)\cr
#' @param FUN the function to be applied. see \link{mapply}.
#' @param ... arguments to vectorize over. see \link{mapply}.
#' @param MoreArgs a list of other arguments to FUN. see \link{mapply}.
#' @param SIMPLIFY see \link{mapply}.
#' @param USE.NAMES see \link{mapply}.
#' @inheritParams lx.lapply
#' @note
#' default value for  \code{pg.verbose} is taken from \link{lx.options}
#' and may therefore be controlled globally.
#' @note
#' default values for \code{SIMPLIFY} and \code{USE.NAMES} is \code{TRUE}
#' as in \link{mapply}, this usually slow down process considerably,
#' you should consider using \code{FALSE} instead to speed up computations.
#' @note
#' there is no mc.allow.recursive option in parallel::mcmapply.
#' don't ask me why... but the consequence is that the behavior
#' in case of a recursive call in child process is udefined.
#' @note
#' there is no progress bar flavor yet
#'
#' @examples
#' n <- 100
#' system.time(x <- lx.mapply(function(x, y) rnorm(5000*(x+y)), 1:n, 1:n))
#' system.time(x <- lx.mapply(function(x, y) rnorm(5000*(x+y)), 1:n, 1:n, use.threads=TRUE))
#'
lx.mapply <- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=TRUE, USE.NAMES=TRUE,
                                   use.threads=lx.use.threads(), 
                                   mc.cores=lx.options("mc.cores"),
                                   mc.preschedule=TRUE) {
  if (use.threads) {
    parallel::mcmapply(FUN, ..., MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY, USE.NAMES=USE.NAMES,
                                   mc.cores=mc.cores,
                                   mc.preschedule=mc.preschedule)
  }
  else {
    mapply(FUN, ..., MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY, USE.NAMES=USE.NAMES)
  }
}

# ---------------------------------------------
#' mapply with names
#' @description
#' \code{lx.napply(X, FUN, ...} is functionally equivalent
#' to: \cr
#' \code{lx.mapply(FUN, names(X), X, ...)}\cr
#' and ensures that names(X) is not NULL\cr
#' FUN should therefore take at least two arguments FUN(name, x, ...)\cr
#' @param X an array
#' @inheritParams lx.mapply
#' @note
#' as an exception in the \code{lx.?apply} family, the default behavior is no thread
#' @note
#' default values for \code{SIMPLIFY} and \code{USE.NAMES} is \code{TRUE}
#' as in \link{lx.mapply}, this usually slow down process considerably,
#' you should consider using \code{FALSE} instead to speed up computations.
#' @examples
#' ll <- list(a=1, b=2, c=3)
#' lx.napply(ll, function(nam, val) paste(nam, val, sep="."))
#'
lx.napply <- function(X, FUN, ..., use.threads=FALSE) {
  nam <- names(X)
  if (is.null(nam))
    nam <- rep("", length(X))
  lx.mapply(FUN, nam, X, ..., use.threads=use.threads)
}

# ---------------------------------------------
#' apply function on each row of a data.frame (or matrix).
#' @description
#' \link{apply} sucks because it coerces its input to an array
#' before applying function. In case of a data.frame this will
#' coerce all data to the same type (usually character).\cr
#' this function performs as \code{apply(X, 1, FUN, ...)}
#' but passing a correctly typed (and named) row to FUN.\cr
#' In addition it can be multithreaded as \link{lx.lapply}.\cr
#' Note that there is no \code{'lx.colapply'} equivalent 
#' since \code{lapply} (and \code{lx.lapply}) will usually 
#' do the (per column) job on a dataframe.
#' @param X an array
#' @param FUN the function to be applied. see \link{lapply}.
#' @param ... anything passed to FUN. see \link{lapply}.
#' @param as.list if TRUE return result as a list of FUN results per row\cr
#' else returns a data.frame or a vector (see below)
#' @param SIMPLIFY if result has only one column convert it to vector
#' @param stringsAsFactors should character columns be converted to factors
#' @inheritParams lx.lapply
#' @return if \code{as.list==TRUE} returns a list else returns 
#' a data.frame or a vector if there is only one column and SIMPLIFY==TRUE.
#' @note
#' when \code{as.list==FALSE} (default) all results from \code{FUN}
#' should be of the same size to be properly rehaped to a data.frame
#' @examples
#' data(iris)
#' x <- lx.rowapply(iris, function(x) x$Sepal.Width * x$Sepal.Length)
#' identical(x, iris$Sepal.Width * iris$Sepal.Length)
#' x <- lx.rowapply(iris, function(x) list(area=x$Sepal.Width * x$Sepal.Length, 
#'                                         name=x$Species))
#'
lx.rowapply <- function(X, FUN, ..., 
                        as.list=FALSE,
                        SIMPLIFY=TRUE,
                        stringsAsFactors=default.stringsAsFactors(),
                        pg.verbose=lx.options("pg.verbose"),
                        use.threads=lx.use.threads(),
                        mc.cores=lx.options("mc.cores"),
                        mc.preschedule=TRUE,
                        mc.allow.recursive=FALSE) {
  
  if (length(dim(X)) != 2) {
    lx.warn("dim(X) must be of length 2")
    return(NULL)
  }
  
  res <- lx.lapply(seq_len(nrow(X)), function(irow) {
    row <- X[irow,]
    FUN(row, ...)
  }, pg.verbose=pg.verbose, use.threads=use.threads,
     mc.cores=mc.cores, mc.preschedule=mc.preschedule,
     mc.allow.recursive=mc.allow.recursive)
  
  if (as.list) return(res)
  
  # reshape as dataframe
  
  res <- as.data.frame(do.call(rbind, res),
                       stringsAsFactors=stringsAsFactors)
  
  # unlist each column
  
  res <- as.data.frame(lapply(res, unlist, recursive=F, use.names=F),
                       stringsAsFactors=stringsAsFactors)

  if (SIMPLIFY && (ncol(res) == 1))
    res <- res[,1]
  
  res
}

# ---------------------------------------------
#' Filter wrapper
#' @description
#' just like \link{Filter} but using \link{lx.lapply}
#' internally.
#' @inheritParams lx.lapply
#' @examples
#' lx.options(pg.verbose=TRUE)
#' lx.Filter(function(x) x%%2==0, 1:10)
#' lx.options(pg.verbose=FALSE)
#' lx.Filter(function(x) x%%2==0, 1:10)
#'
lx.Filter <- function(FUN, X,
                      pg.verbose=lx.options("pg.verbose"),
                      use.threads=lx.use.threads(), 
                      mc.cores=lx.options("mc.cores"),
                      mc.preschedule=TRUE,
                      mc.allow.recursive=FALSE) {

    ind <- as.logical(unlist(lx.lapply(X, FUN,
                                       pg.verbose=pg.verbose,
                                       use.threads=use.threads, 
                                       mc.cores=mc.cores,
                                       mc.preschedule=mc.preschedule,
                                       mc.allow.recursive=mc.allow.recursive)))
    X[!is.na(ind) & ind]
}

# ---------------------------------------------
#' Map wrapper
#' @description
#' just like \link{Map} but using \link{lx.mapply}
#' internally\cr
#' @inheritParams lx.mapply
#' 
lx.Map <- function (FUN, ..., use.threads=lx.use.threads()) {
    FUN <- match.fun(FUN)
    lx.mapply(FUN=FUN, ..., SIMPLIFY = FALSE, use.threads=use.threads)
}

# -------------------------------------------------
#' lx.lapply with duplicated file handle
#' @description
#' a version of \link{lx.lapply} that duplicates the file handle
#' \code{handle} in each thread.
#' this is useful when \code{FUN} needs read access to file since
#' the file descriptor will be different in each thread.\cr
#' warning: don't use for write access since the write position
#' is unpredictable.
#' @inheritParams lx.lapply
#' @param handle a file handle (see \link{HELP.FILE.HANDLE})\cr
#' @note
#' user's function \code{FUN} has the form \code{FUN(x, handle, ...)}
#'
lx.happly <- function(X, FUN, handle, ..., pg.verbose=lx.options("pg.verbose"),
                                   use.threads=lx.use.threads(),
                                   mc.cores=lx.options("mc.cores")) {

   # no thread
   if (! use.threads)
     return(lapply(X, FUN, handle, ...))
     
   #
   # split input list into 'mc.cores' chunks
   #
   len    <- length(X)
   lx.out("splitting ", len, " elements by ", mc.cores, " threads", level="debug")
   chunks <- rep(1:mc.cores, length.out=len)

   X <- split(X, chunks)
   
   #
   # run mcapply on each chunk in turn (i.e. with 1 core per chunk)
   #
   res <- parallel::mclapply(X, function(y, handle, ...) {
   
     # duplicate file handle
     handle <- lx.dup.handle(handle)

     # run user function in each thread
     lx.out("running FUN in thread", level="debug")
     res <- lapply(y, FUN, handle, ...)
     
     # close dup file handle
     lx.close(handle)
     
     res
   }, handle, ..., mc.cores=mc.cores,
                   mc.preschedule=FALSE,
                   mc.allow.recursive=FALSE)
   
   #
   # flatten results
   #
   lx.out("unsplitting chunks", level="debug")
   unsplit(res, chunks)
}
