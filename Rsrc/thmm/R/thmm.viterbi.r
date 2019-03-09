# -------------------------------------------------
# $Id: thmm.viterbi.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : viterbi algorithm
#

# -------------------------------------------------
#' Viterbi algorithm
#' @description This function calculates the optimal hidden states sequence
#' using Viterbi's algorithm.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param with.probseq should we compute \code{logprobseq and loglike} (see value)
#' @param .useC logical if TRUE (default) use C code else use R code
#' @details the purpose of the Viterbi's algorithm is to determine
#' the sequence of states \code{(k_1*, ..., k_n*)} which maximises
#' the joint distribution of the hidden states given the entire
#' observed process. i.e:\cr
#' \code{(k_1*, ..., k_n*) = argmax(Pr(state_1=k_1, ..., state_n=k_n, X_1=obs_1, ..., X_n=obs_n | hmm))}
#'
#' @return list with five components: \code{states}, \code{lognu}, \code{logviterbi}
#' \code{loglike} and \code{logprobseq}
#' \itemize{
#' \item\code{states} : vector of optimal states \code{(k_1*, ..., k_n*)}
#'
#' \item\code{lognu} : the (log of) joint probabilities (densities) matrix:\cr
#' \code{nu_{i+1,j} = Pr(state_1=k_1*, ..., state_i=k_i*, state_i+1 = j,  X_1=obs_1, ..., X_i=obs_i | hmm)}
#' where k_i* is the optimal state at time i.
#'
#' \item\code{logviterbi} : log of Viterbi's score, i.e.
#' the log of joint probability (density) of the optimal sequence of states
#' and observations with this hmm\cr
#' \code{log(Pr(state_1=k_1*, ..., state_n=k_n*, obs | hmm))}\cr
#' \code{=max_j(lognu{n,j})}
#'
#' \item\code{loglike} : the log-likelihood of this observation with this hmm:\cr
#' \code{log(Pr{ X_1 = obs_1, ..., X_n = obs_n | hmm})}=log(Pr(obs | hmm))
#' (see \link{thmm.forward})\cr
#' this value is NA if \code{with.probseq == FALSE}
#'
#' \item\code{logprobseq} : log of probability (density) of the optimal sequence
#' of states conditionally to the observations. i.e\cr
#' \code{log(Pr(state_1=k_1*, ..., state_n=k_n* | obs, hmm))}\cr
#' \code{=log(Pr(state_1=k_1*, ..., state_n=k_n*, obs | hmm) / Pr(obs | hmm))}\cr
#' \code{=logviterbi - loglike}
#' }
#' this value is NA if \code{with.probseq == FALSE}
#' @note
#' the code takes care of rescaling values during calculation
#' to avoid underflow problems and is quite robust in practice.
#' @note
#' for multivariate HMM, \code{obs} should be a list of (numerical)
#' tuples. (see \link{thmm} examples).
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
#' vit <- thmm.viterbi(hmm, obs, with.probseq=TRUE)
#' table(vit$states)
#' #
#' # test numerical stability
#' #
#' obs[101] <- 1000000  # this may creates overflow
#' vit <- thmm.viterbi(hmm, obs, with.probseq=TRUE)
#' table(vit$states)
#' \dontrun{
#' plot(vit$states)}
#' @seealso \link{thmm.forward}
#'
thmm.viterbi <- function(hmm, obs, with.probseq=FALSE, .useC=TRUE) {
  n <- length(obs)

  vit <- if (.useC) .C.viterbi(hmm, obs)
  else .R.viterbi(hmm, obs)

  logviterbi <- if (n > 0) max(vit$nu[n,]) else NA

  if (with.probseq) {
    loglike <- thmm.forward(hmm, obs, .useC=.useC)$loglike
    logprobseq <- logviterbi - loglike
  } else {
    loglike <- logprobseq <- NA
  }

  list(states=vit$states, nu=vit$nu, logviterbi=logviterbi,
       loglike=loglike, logprobseq=logprobseq)
}

