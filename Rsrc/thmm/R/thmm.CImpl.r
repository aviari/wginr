# -------------------------------------------------
# $Id: thmm.CImpl.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : interface to C implementation
# @seealso: thmm.RImpl.r for a pure R implementation
#

# -------------------------------------------------
# forward
#
.C.forward <- function(hmm, obs) {
    n <- length(obs) ## check n > 0
    m <- nrow(hmm$trans)
    logalpha <- matrix(0, nrow=n, ncol=m)
    logdens <- .logdens(hmm, obs)
    res <- .C("C_forward", n, m, logdens, hmm$trans, hmm$init,
              logalpha=logalpha, loglike=double(1), NAOK=T)
    list(logalpha=res$logalpha, loglike=ifelse(n>0, res$loglike, NA))
}

# -------------------------------------------------
# backward
#
.C.backward <- function(hmm, obs) {
  n <- length(obs) ## check n > 0
  m <- nrow(hmm$trans)
  logbeta <- matrix(0, nrow=n, ncol=m)
  logdens <- .logdens(hmm, obs)
  res <- .C("C_backward", n, m, logdens, hmm$trans,
            logbeta=logbeta, NAOK=T)
  list(logbeta=res$logbeta)
}

# -------------------------------------------------
# viterbi
#
.C.viterbi <- function(hmm, obs) {
  n <- length(obs) ## check n > 0
  m <- nrow(hmm$trans)
  nu <- matrix(0, nrow=n, ncol=m)
  states <- integer(n)
  logdens <- .logdens(hmm, obs)
  logtrans <- log(hmm$trans)
  res <- .C("C_viterbi", n, m, logdens, logtrans, hmm$init,
            nu=nu, states=states, NAOK=T)
  list(states=res$states, nu=res$nu)
}


