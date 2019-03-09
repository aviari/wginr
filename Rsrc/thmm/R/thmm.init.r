# -------------------------------------------------
# $Id: thmm.init.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : initialization, print
#

# -------------------------------------------------
#' initialize HMM
#' @description initialize a discrete time hidden markov model
#' (DTHmm)
#' @param density a probability density function (pdf) such as \link{dnorm}
#' (see notes)
#' @param trans the transition matrix given either as a matrix, a vector
#' or a constant (see details)
#' @param init vector of probabilities of the initial states (see details)
#' @param ... parameters of \code{density} (see examples)
#' @return a DTHmm object describing the DTHmm
#' @details
#' the number of states (m) is determined from the ... arguments (see notes)
#' and should be consistent with the value passed to \code{trans} and
#' \code{init} if provided.
#'
#' if \code{trans} is a constant then the transition matrix
#' is computed as a mxm matrix with \bold{off-diagonal} elements \code{trans}.
#'
#' if \code{trans} is a vector (it should then be of length m) then the
#' transition matrix is computed as a mxm matrix with \bold{diagonal}
#' elements \code{trans}.
#'
#' if \code{init} is NULL then \code{init} is computed as a vector
#' of uniform probabilities (1/m).
#'
#' if \code{init} is not of length m then it is recycled/shortened to proper length m.
#'
#' @note
#' the pdf function \code{density} has the form:\cr
#' \code{density(x, ..., log=TRUE|FALSE)}\cr
#' see \link{dnorm}, \link{dpois}, \link{dbeta}, etc.
#' as examples but you may write your own as well, with the following specifications:\cr
#' \itemize{
#' \item \code{density} parameters must have default values if you don't specify
#' them in \code{...}
#' \item \code{density} must accepts vectors (including of NA's) as parameters
#' (possibly recycling them) (each parameter corresponds to each state) and return
#' a vector of length m (possibly of NA's) when called with a scalar \code{x}
#' (including NA). this is used to determine the actual number of states m.
#' \item (not mandatory) It may accept a vector (including of NA's) as \code{x} argument
#' (with individual states parameters) and return a vector of size \code{length(x)}.
#' this is used by the C code only, so if this condition is not satisfied the C code will
#' workaround but will run a bit slower (albeit still quicker than R code).
#' }
#' You may pass the function symbol or function name (character) as \code{density}
#' argument (but not a direct lambda expression nor anything fancy).
#'
#' @note
#' For multivariate HMM, the density function should accept a list of
#' (numeric) tuples as \code{x} argument (or anything equivalent like
#' a list of complex numbers for bivariate). The parameters can be
#' anything relevant to your mutivariate density distribution but
#' may be received as lists (one element for each state) instead
#' of vectors. See examples in \link{thmm} for details.
#'
#' @examples
#' # ---------------------------------------
#' # Normal (gaussian) HMM with two states
#' # full specification
#' trans <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow=2)
#' init <- c(0.5, 0.5)
#' hmm <- thmm.init(dnorm, trans, init, mean=c(-1,1), sd=c(0.1, 0.1))
#'
#' # ---------------------------------------
#' # *same as previous* with simplified call
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#'
#' # ---------------------------------------
#' # three states HMM with Poisson distribution
#' hmm <- thmm.init(dpois, 0.1, lambda=1:3)
#'
#' # ---------------------------------------
#' # two states HMM with Gamma distribution
#' hmm <- thmm.init(dgamma, 0.1, shape=c(3, 10), rate=c(1, 2))
#'
#' # ---------------------------------------
#' # mixture of gaussians
#' # state1: mixture of 2 gaussians with mean=-1,1 sd=0.1,0.1 and prob=0.1,0.9
#' # state2: gaussian mean=0 sd=0.2
#' #
#' # we define first \code{dmixnorm} as the pdf of a mixture of gaussians
#' # note that this function is 'vectorized' on parameters (and x either).
#' # the computation with log=TRUE will avoid underflow
#' #
#' dmixnorm <- function(x, mean=0, sd=1, prob=1, log=FALSE) {
#'   msp <- mapply(function(m, s, p) list(mean=m, sd=s, prob=p), mean, sd, prob, SIMPLIFY=FALSE)
#'   res <- if (log) {
#'     lapply(x, function(x) sapply(msp, function(p) {
#'       pp <- base::log(p$prob) + dnorm(x, mean=p$mean, sd=p$sd, log=TRUE)
#'       mp <- max(pp)
#'       mp + base::log(sum(exp(pp-mp)))
#'     }))
#'   } else {
#'     lapply(x, function(x) sapply(msp, function(p)
#'       sum(p$prob*dnorm(x, mean=p$mean, sd=p$sd, log=FALSE))))
#'   }
#'   unlist(res)
#' }
#' #
#' # then the hmm
#' hmm <- thmm.init(dmixnorm, 0.1, mean=list(c(-1,1), 0), sd=list(0.1, 0.2),
#'                                 prob=list(c(0.5,0.5), 1))
#'
#' # ---------------------------------------
#' # discrete distribution : binomial
#' #
#' # dbinom does not have default parameters
#' # so we need to specify all of them
#' hmm <- thmm.init(dbinom, 0.1, size=c(50, 100), prob=c(0.5, 0.1))
#'
#' # ---------------------------------------
#' # discrete distribution : the dishonest casino
#' #
#' # we first define the pdf
#' #
#' dice <- function(x, p6=1/6, log=FALSE) {
#'   r <- unlist(lapply(x, function(x) {
#'     unlist(lapply(p6, function(p6) {
#'       if (is.na(x)) NA
#'       else if (x == 6) p6
#'       else if (x %in% 1:5) (1-p6)/5
#'       else 0
#'     })) }), recursive=FALSE)
#'   if (log) r <- base::log(r)
#'   r
#' }
#' # and the random generator (d->r)ice (see \link{thmm.simulate})
#' rice <- function(n, p6=1/6) {
#'   sample(1:6, n, prob=c(rep((1-p6)/5, 5), p6), replace=TRUE)
#' }
#'
#' hmm <- thmm.init(dice, 0.3, p6=c(1/6, 3/6))
#' obs <- thmm.simulate(hmm, 100)
#' \dontrun{
#' plot(obs$values)
#' }
#
thmm.init <- function(density, trans, init=NULL, ...) {
  hmm <- .new("DTHmm")

  # get density function as name or symbol
  #   density.name : function name
  #   density.fun  : function
  if (is.character(density)) {
    density.name <- density
    density.fun  <- get(density, mode="function", envir=parent.frame())
  } else if (is.function(density)) {
    density.name <- deparse(substitute(density))
    density.fun  <- density
  } else {
    density.name <- NULL
  }

  # check if density.name is valid (i.e. not a lambda or whatever exotic)
  if (! identical(density.name, make.names(density.name)))
    stop("'density' should be either a function symbol or function name")

  # 'hmm$density' will call density function as a closure (with *all* parameters)
  # i.e. hmm$density(x) gives the probability densities at x
  # this is mostly used in R Code. C Code use informations in hmm$info (see below)
  hmm$density <- function(x, log=TRUE) density.fun(x, ..., log=log)

  # get number of states by calling hmm$density(NA)
  ns <- length(hmm$density(NA))

  # transition matrix
  hmm$trans <- if (is.matrix(trans)) trans else .transmat(trans, ns)
  hmm$trans <- hmm$trans / rowSums(hmm$trans)
  if (ns != nrow(hmm$trans)) stop("inconsistent transition matrix size")
  storage.mode(hmm$trans) <- "double"
  if (any(is.na(hmm$trans) | hmm$trans < 0))
    stop("transition matrix contains negative or NA entries")
  if (any(diag(hmm$trans) <= .Machine$double.xmin))
    warning("some diagonal entries of transition matrix are zero")

  # initial probabilities
  hmm$init  <- if (is.null(init)) 1/ns else init
  hmm$init  <- head(rep(hmm$init, ns), ns)
  hmm$init  <- hmm$init / sum(hmm$init)
  storage.mode(hmm$init) <- "double"
  if (any(is.na(hmm$init) | hmm$init < 0))
    stop("initial probabilities contain negative or NA values")

  # keep useful information (mostly for C calls)
  #  info$density    : density function name
  #  info$args       : density arguments as a single list
  #  info$sargs      : density arguments splitted by states
  #  info$env        : environment needed to call info$density
  #  info$vectorized : tell if density can be vectorized
  #
  # note: sargs is useful to access individual state parameters as hmm$info$sargs[[istate]]
  #  e.g.: do.call(hmm$info$density, c(x=list(x), hmm$info$sargs[[istate]]), envir=hmm$info$env)
  #        return the probability density at x for state istate
  #
  #  e.g.: sapply(hmm$info$sargs, function(x) x$mean) gives
  #        the 'mean' parameters (of normal DTHmm) for all states
  #

  # force evaluation of density parameters ('...') in the context of the caller
  args <- eval(substitute(list(...)), envir=parent.frame())

  hmm$info <- list(density=density.name,
                   args=list(args),
                   env=parent.frame())

  # and split them by state (recycling if necessary)
  hmm$info$sargs <- lapply(1:ns, function(i)
    lapply(args, function(x) .cycle(x,ns)[i]))

  # more in-depth check of density
  # these two conditions have eventually been removed
  # the first one is too boring for the user
  # the second one is not so easy to check if the first one is not satisfied
  #
  #     # check if density has proper default parameters
  #     if ("symbol" %in% sapply(tail(formals(density), -1), typeof))
  #       stop("'density' does not have default for all parameters")
  #
  #     # check if density is properly vectorized on parameters
  #     chk <- tryCatch(do.call(hmm$info$density, list(NA, rep(NA, ns)), envir=hmm$info$env),
  #                     error=function(e) NULL)
  #     if (length(chk) != ns) stop("'density' is not properly vectorized on parameters")
  #

  # finally check if density is vectorized on first argument
  chk <- tryCatch(lapply(hmm$info$sargs, function(arg)
    do.call(hmm$info$density, c(list(rep(NA, 10)), arg), envir=hmm$info$env)),
    error=function(e) NA)
  hmm$info$vectorized <- all(sapply(chk, length) == 10)
  if (! hmm$info$vectorized)
    warning("'density' is not vectorized on x: this may slow down some computations")

  hmm
}

# -------------------------------------------------
#' print method for DTHmm
#' @param x a DTHmm object
#' @param ... further arguments passed to or from other methods
#' @return invisible(x)
#
print.DTHmm <- function(x, ...) {
  .f <- function(x) format(unclass(x))
  .p <- function(...) print(paste(..., sep=" ", collapse=""), quote=F)
  .p("-------------------------------------")
  .p("<Discrete Time HMM>")
  .p("-------------------------------------")
  .p("nb.states :", nrow(x$trans))
  .p("density   :", x$info$density)
  .p("args      :", x$info$args)
  .p("init      :", paste(.f(x$init), collapse=" "))
  .p("trans     :")
  apply(.f(x$trans), 1, function(x) .p(" ", x))
  .p("-------------------------------------")
  invisible(x)
}

# -------------------------------------------------
#' get hmm parameters
#' @description get hmm global and individual states parameters
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param what character parameter name to retrieve (all parameters if NULL (default))
#' @return if \code{what==NULL} return a list of 5 components: \code{nstates},
#' \code{density}, \code{args}, \code{init}, \code{trans}
#' \itemize{
#' \item nstates : integer number of states
#' \item density : character string name of probability density function
#' \item args : list (of length nstates) of named lists, each element
#' gives the density function parameters for state i. (see note).
#' \item init : numerical vector of initial probabilities
#' \item trans : numerical transition matrix
#' }
#' if \code{what!=NULL} then what should be either one of the above names or
#' the name of one of the density function parameters. the function then return
#' only this information.
#' @examples
#' # univariate gaussian hmm
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=1)
#' ns <- thmm.parameters(hmm, "nstates")
#' means <- thmm.parameters(hmm, "mean")
#' # viterbi decode
#' obs <- rnorm(100)
#' vit <- thmm.viterbi(hmm, obs)
#' # as values
#' means[vit$states]
#'
thmm.parameters <- function(hmm, what=NULL) {
  res <- list(nstates=nrow(hmm$trans),
              density=hmm$info$density,
              args=hmm$info$sargs,
              env=hmm$info$env,
              init=hmm$init,
              trans=hmm$trans)
  if (is.null(what)) res
  else if (what %in% names(res)) res[[what]]
  else sapply(res$args, function(x) x[[what]])
}

# -------------------------------------------------
#' forget internal environment
#' @description
#' In the current implementation a \code{DTHmm} object
#' keeps internally tracks of the \link{parent.frame} environment where
#' \link{thmm.init} has been called. This is necessary to ensure that the
#' \code{...} arguments will be interpreted in the correct context.
#' A drawback arise when you want to serialize (thru \link{serialize}, \link{save}
#' or \link{saveRDS}) this object. R will also serialize this environment and
#' this may gives rise to a potentially huge file (if you have large objects
#' in this environment).\cr
#' For now, a workaround is to replace this environment by \link{globalenv}.
#' This is safe if you do not refer to any parent frame variable in the
#' code of \code{density} (which is a good programming
#' practice). A (temporary) helper function \link{thmm.forget.env} is provided
#' to perform this replacement. You may call it just before serializing.\cr
#' This unfortunate behavior will be changed in the future. I just need a bit
#' of time to think about possible consequences of forgetting parent frame.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @return modified DTHmm
#' @examples
#' \dontrun{
#' myhmm <- function() { HUGE=rnorm(1e6); thmm.init(dnorm, 0.1, mean=c(-1,1), sd=1) }
#' hmm0 <- myhmm()
#' saveRDS(hmm0, "hmm0.rds")  # 7 Mb
#' hmm1 <- thmm.forget.env(hmm0)
#' saveRDS(hmm1, "hmm1.rds")  # 18 Kb
#' hmm1 <- readRDS("hmm1.rds")
#' obs <- thmm.simulate(hmm1, 100, .seed=0)
#' vit <- thmm.viterbi(hmm1, obs$values)
#' plot(vit$states)
#' points(obs$states, pch=16, cex=0.5, col=2)
#' }
#'
thmm.forget.env <- function(hmm) {
  if (! is.null(hmm)) {
    hmm$info$env <- globalenv()
    environment(hmm$density) <- globalenv()
  }
  hmm
}


