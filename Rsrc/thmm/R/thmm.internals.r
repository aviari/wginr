# -------------------------------------------------
# $Id: thmm.internals.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : internal utilities
# all these functions are internal - not exported
#

# -------------------------------------------------
# <internal> check if e is an error
#
.is.error <- function(e) "error" %in% class(e)

# -------------------------------------------------
# <internal> stop on error
#
.stopif <- function(test, ...) if (test) stop(...)

# -------------------------------------------------
# <internal> iota function
#
.iota  <- function(n) seq.int(length.out=max(0, n))

# -------------------------------------------------
# <internal> recycle vector
#
.cycle <- function(x, n) head(rep(x, n), n)

# -------------------------------------------------
# <internal> create new object
#
.new <- function(classname, ...) {
  obj <- list(...)
  class(obj) <- c(classname, class(obj))
  obj
}

# -------------------------------------------------
# column sums of matrix
# same as apply(mat, 2, sum) but twice quicker
#
.colSums <- function(mat) rowSums(t(mat))

# -------------------------------------------------
# <internal> get function actual arguments
# pos : index of argument(s) to extract (starts at 1 with function name)
# up.frame : number of frame to go up to get caller
#
.args <- function(pos=NULL, up.frame=0L) {
  if ((sys.nframe() - up.frame) < 2) # called from top level
    return(NULL)

  parent <- sys.parent(up.frame+1)
  def    <- sys.function(parent)
  cal    <- sys.call(parent)

  # trouble when called within a lambda expression [fixme]
  args <- as.list(tryCatch(
                    match.call(definition=def, call=cal),
                    error=function(e) "closure"))

  if (is.null(pos)) args else args[pos]
}

# -------------------------------------------------
# <internal> printout debug information on stderr
#
.out <- function(..., with.mem=FALSE, with.caller=TRUE, up.frame=0L) {

  mem <- if (with.mem) paste0("[", format(sum(gc()[,2]), nsmall=1), " Mb] ")
         else ""

  cal <- if (with.caller) {
    caller <-.args(1, up.frame=up.frame+1)[[1]]
    caller <- if (is.null(caller)) "top-level"
    else if (typeof(caller) == "closure") "closure"
    else paste(as.character(caller), collapse=":")
    paste0("[", caller, "] ", collapse="")
  } else ""

  args <- paste0(as.character(list(...)), collapse="")

  cat(paste0("+ [", date(), "]\t", cal, args, " ", mem, "\n"),
      file=stderr())

  flush(stderr())

  invisible()
}

# -------------------------------------------------
# <internal> make transition matrix from constant or vector
#
.transmat <- function(p, m) {
  p <- pmin(1, pmax(0, p))
  if (length(p) == 1) {
    x <- matrix(p, nrow=m, ncol=m)
    diag(x) <- 1 - (m-1)*p
  } else {
    p <- .cycle(p, m)
    x <- matrix(1-p, nrow=m, ncol=m)
    diag(x) <- (m-1)*p
    x <- x / rowSums(x)
  }
  x
}

# -------------------------------------------------
# <internal> compute log(density(obs))
# the first form is much quicker but assume that density
# accepts a vector as first argument
#
.logdens <- function(hmm, obs) {
  if (hmm$info$vectorized) {
    res <- lapply(hmm$info$sargs, function(arg)
      do.call(hmm$info$density, c(list(obs), arg, log=TRUE), envir=hmm$info$env))
    do.call(cbind, res)
  } else {
    matrix(unlist(lapply(obs, hmm$density, log=TRUE)), ncol=nrow(hmm$trans), byrow=TRUE)
  }
}

# -------------------------------------------------
# <internal> marginal distribution of stationary Markov chain
#
.marginprob <- function(trans) {
  m <- ncol(trans)
  a <- t(trans) - diag(1, m, m)
  a[m,] <- 1
  b <- c(rep(0, (m - 1)), 1)
  solve(a, b)
}
