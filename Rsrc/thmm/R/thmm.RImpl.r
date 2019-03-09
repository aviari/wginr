# -------------------------------------------------
# $Id: thmm.RImpl.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : pure R implementation
# @seealso: thmm.CImpl.r for a C implementation
#

# -------------------------------------------------
# (log) forward probabilities
#
.R.forward <- function(hmm, obs) {
  n <- length(obs)
  logalpha <- matrix(NA, nrow=n, ncol=nrow(hmm$trans))
  logscale <- if (n==0) NA else 0  # makes sure loglike is NA when n = 0
  phi <- hmm$init
  for (i in .iota(n)) {
    logphi <- log(phi) + hmm$density(obs[i], log=TRUE)
    logalpha[i,] <- logphi + logscale
    # update logscale without underflow
    maxlog <- max(logphi)
    logsum <- maxlog + log(sum(exp(logphi-maxlog)))
    logscale <- logscale + logsum
    # renormalise phi and prepare for next iteration
    phi <- exp(logphi - logsum) %*% hmm$trans
  }
  list(logalpha=logalpha, loglike=logscale)
}

# -------------------------------------------------
# (log) backward probabilities
#
.R.backward <- function(hmm, obs) {
  n <- length(obs)
  m <- nrow(hmm$trans)
  logbeta <- matrix(0, nrow=n, ncol=m) # this ensures logbeta[n,] = 0
  logscale <- log(m)
  phi <- rep(1/m, m)
  for (i in rev(.iota(n-1))) {
    logphi <- log(phi) + hmm$density(obs[i+1], log=TRUE)
    # work with rescaled phi = exp(logphi-maxlog) to avoid underflow
    maxlog <- max(logphi)
    phi <- hmm$trans %*% exp(logphi-maxlog)
    logbeta[i,] <- log(phi) + maxlog + logscale
    # update logscale and renormalise phi
    sumphi <- sum(phi)
    logscale <- logscale + log(sumphi) + maxlog
    phi <- phi / sumphi
  }
  list(logbeta=logbeta)
}

# -------------------------------------------------
# Viterbi algorithm
#
.R.viterbi <- function(hmm, obs) {
  n <- length(obs)
  m <- nrow(hmm$trans)

  nu <- matrix(NA, nrow=n, ncol=m)
  logtrans <- log(hmm$trans)

  if (n > 0) nu[1,] <- pnu <- log(hmm$init) + hmm$density(obs[1], log=TRUE)

  for (i in tail(.iota(n), -1)) {
    mu <- matrix(pnu, nrow=m, ncol=m, byrow=FALSE) + logtrans
    nu[i,] <- pnu <- apply(mu, 2, max) + hmm$density(obs[i], log=TRUE)
  }

  states <- integer(n)
  if (n > 0) prev <- states[n] <- which.max(nu[n,])
  for (i in rev(.iota(n-1))) {
    states[i] <- prev <- which.max(logtrans[,prev] + nu[i,])
  }

  list(states=states, nu=nu)
}



