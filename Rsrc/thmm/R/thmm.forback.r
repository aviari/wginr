# -------------------------------------------------
# $Id: thmm.forback.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : forward-backward algorithm
#


# -------------------------------------------------
#' (log) forward probabilities
#' @description compute forward probabilities of observations
#' according to a discrete time hidden markov model (DTHmm).
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param .useC logical if TRUE (default) use C code else use R code
#' @return list with two components: \code{logalpha} and \code{loglike}
#' \itemize{
#' \item\code{logalpha} : the (log of) forward probabilities (densities) matrix:\cr
#' \code{alpha_{i,j} = Pr{ X_1 = obs_1, ..., X_i = obs_i, state_i = j | hmm}}
#' that is the probability (density) of seeing the partial sequence
#' \code{(obs_1, ..., obs_i)} and ending up in state j at time i for this
#' hmm.
#'
#' \item\code{loglike} : the log-likelihood of this observation with this hmm:\cr
#' \code{log(Pr{ X_1 = obs_1, ..., X_n = obs_n | hmm})}=\code{max_j(logalpha_{n,j}}
#' }
#' @note
#' the code takes care of rescaling values during calculation
#' to avoid underflow problems and is quite robust in practice.
#' @note
#' since the computation uses density probabilities it is perfectly
#' valid to get a positive log-likelihood.
#' @note
#' for multivariate HMM, \code{obs} should be a list of (numerical)
#' tuples. (see \link{thmm} examples).
#'
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
#' fwd <- thmm.forward(hmm, obs)
#' #
#' # test numerical stability
#' #
#' dif1 <- fwd$logalpha[-100,1] - fwd$logalpha[-100,2]
#' obs[100] <- 1000000  # this may creates overflow
#' fwd <- thmm.forward(hmm, obs)
#' dif2 <- fwd$logalpha[-100,1] - fwd$logalpha[-100,2]
#' max(abs((dif1-dif2)/dif1))
#' \dontrun{
#' plot(dif1)
#' points(dif2, col=2, cex=0.1)}
#' @seealso \link{thmm.backward}, \link{thmm.forward.backward}
#'
thmm.forward <- function(hmm, obs, .useC=TRUE) {
  if (.useC) .C.forward(hmm, obs)
  else .R.forward(hmm, obs)
}

# -------------------------------------------------
#' (log) backward probabilities
#' @description compute backward probabilities of observations
#' according to a discrete time hidden markov model (DTHmm).
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param .useC logical if TRUE (default) use C code else use R code
#' @return list with one components: \code{logbeta}
#' (for symmetry with \link{thmm.forward})
#' \itemize{
#' \item\code{logbeta} : the (log of) backward probabilities (densities) matrix:\cr
#' \code{beta_{i,j} = Pr{ X_{i+1} = obs_{i+1}, ..., X_n = obs_n | state_i = j , hmm}}
#' that is the probability (density) of the ending partial sequence
#' \code{(obs_i+1, ..., obs_n)} given that we started at state j at time i,
#' for this hmm.
#' }
#' @note
#' the code takes care of rescaling values during calculation
#' to avoid underflow problems and is quite robust in practice.
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
#' bwd <- thmm.backward(hmm, obs)
#' #
#' # test numerical stability
#' #
#' dif1 <- bwd$logbeta[-c(200,99),1] - bwd$logbeta[-c(200,99),2]
#' obs[100] <- 1000000  # this may creates overflow
#' bwd <- thmm.backward(hmm, obs)
#' dif2 <- bwd$logbeta[-c(200,99),1] - bwd$logbeta[-c(200,99),2]
#' max(abs((dif1-dif2)/dif1))
#' \dontrun{
#' plot(dif1)
#' points(dif2, col=2, cex=0.1)}
#' @seealso \link{thmm.forward}, \link{thmm.forward.backward}
#'
thmm.backward <- function(hmm, obs, .useC=TRUE) {
  if (.useC) .C.backward(hmm, obs)
  else .R.backward(hmm, obs)
}

# -------------------------------------------------
#' (log) forward/backward probabilities
#' @description compute forward/backward probabilities of observations
#' according to a discrete time hidden markov model (DTHmm).
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param .useC logical if TRUE (default) use C code else use R code
#' @return list with five components: \code{logalpha}, \code{logbeta},
#' \code{gamma}, \code{rho}, \code{loglike}
#' \itemize{
#' \item\code{logalpha} : the (log of) forward probabilities (densities) matrix:\cr
#' \code{alpha_{i,j} = Pr{ X_1 = obs_1, ..., X_i = obs_i, state_i = j | hmm}}
#' that is the probability (density) of seeing the partial sequence
#' \code{(obs_1, ..., obs_i)} and ending up in state j at time i for this
#' hmm.
#'
#' \item\code{logbeta} : the (log of) backward probabilities (densities) matrix:\cr
#' \code{beta_{i,j} = Pr{ X_{i+1} = obs_{i+1}, ..., X_n = obs_n | state_i = j , hmm}}
#' that is the probability (density) of the ending partial sequence
#' \code{(obs_i+1, ..., obs_n)} given that we started at state j at time i,
#' for this hmm.
#'
#' \item\code{gamma} : the probabilities matrix:\cr
#' \code{gamma_{i,j} = Pr{ state_i = j | obs , hmm}}
#' that is the probability of being at time i in state j given
#' this observation and this hmm.
#'
#' \item\code{logrho} : the (log of) probabilities (densities) vector:\cr
#' \code{rho_{i} = Pr{ X_1 = obs_1, ..., X_i = obs_i | hmm}}
#' that is the probability (density) of seeing the partial sequence
#' \code{(obs_1, ..., obs_i)} for this hmm.
#'
#' \item\code{loglike} : the log-likelihood of this observation with this hmm:\cr
#' \code{log(Pr{ X_1 = obs_1, ..., X_n = obs_n | hmm})} (see \link{thmm.forward})
#' }
#' @note
#' the code takes care of rescaling values during calculation
#' to avoid underflow problems and is quite robust in practice.
#' @note
#' by definition \code{loglike = logrho(length(obs))} (although it is not
#' computed this way).
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
#' fb <- thmm.forward.backward(hmm, obs)
#' #
#' # test numerical stability
#' #
#' obs[101] <- 1000000  # this may creates overflow
#' fb <- thmm.forward.backward(hmm, obs)
#' \dontrun{
#' plot(fb$gamma[,2])}
#' @seealso \link{thmm.forward}, \link{thmm.backward}
#'
thmm.forward.backward <- function(hmm, obs, .useC=TRUE) {
  fwd  <- thmm.forward(hmm, obs, .useC=.useC)
  logalpha <- fwd$logalpha
  logbeta  <- thmm.backward(hmm, obs, .useC=.useC)$logbeta

  gamma <- exp(logalpha + logbeta - fwd$loglike)

  imax <- max.col(logalpha)
  logrho <- as.numeric(logalpha[cbind(seq_along(imax), imax)])

  list(logalpha=logalpha, logbeta=logbeta, gamma=gamma, logrho=logrho,
       loglike=fwd$loglike)
}


