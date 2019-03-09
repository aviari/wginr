# -------------------------------------------------
# $Id: thmm.simul.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : simulation
#

# -------------------------------------------------
#' simulate hidden markov process
#' @description generate states and observations vectors following a
#' discrete time HMM
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param npts number of points to simulate
#' @param with.values logical also compute observed values (see details)
#' @param log (if \code{with.values=TRUE}) compute log of values
#' @param .random name of random generation function (see details).
#' @param .seed if not NA set random seed before generation
#' @return if \code{with.values=FALSE} integer vector of simulated states.
#' if \code{with.values=TRUE} a list of two vectors \code{states} and \code{values}.
#' @details if \code{with.values=TRUE} then the random generation function used
#' to produce values from states is \code{'(d->r)ensity'} i.e. the name of density
#' probability function with first letter (usually a 'd') replaced by a 'r'.
#' (e.g. if \code{density=dnorm} then the random function is \code{rnorm})
#' this can be overriden by using the \code{.random} parameter.\cr
#' This function has the following form:\cr
#' \code{random(n, ...)} where \code{...} are the same parameters as \code{density}.\cr
#' In contrast to \code{density}, the generation function does not need to be vectorized
#' on parameters nor \code{n} (actually it is always called with \code{n=1} and individual
#' state parameters).
#' @note neither \code{states} not \code{values} are deterministic, you should
#' call \link{set.seed} to produce reproducible vectors.
#' @note for multivariate HMM, the result is a list of (numeric) tuples (one
#' tuple element per each random variable). See examples in \link{thmm}
#' for details.
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' set.seed(0)
#' thmm.simulate(hmm, 10, log=TRUE)
#'
#'
thmm.simulate <- function(hmm, npts, with.values=TRUE, log=FALSE, .random=NULL, .seed=NA) {
  m <- nrow(hmm$trans)

  if (! is.na(.seed)) set.seed(.seed)

  s0 <- sample(m, 1, prob=hmm$init)
  states <- head(Reduce(function(s, i) sample(m, 1, prob=hmm$trans[s,]),
                        seq.int(length.out=npts), init=s0, accumulate=T), -1)

  if (with.values) {
    rfun <- if (is.null(.random)) sub("^.", "r", hmm$info$density)
    else .random
    args <- thmm.parameters(hmm)$args
    values <- sapply(states, function(i) do.call(rfun, c(1, args[[i]]), envir=hmm$info$env))
    list(states=states, values=values)
  } else
    states
}
