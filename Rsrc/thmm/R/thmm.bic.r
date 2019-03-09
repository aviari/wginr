# -------------------------------------------------
# $Id: thmm.bic.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : BIC, AIC
#

# -------------------------------------------------
#' compute AIC
#' @description compute Akaike's Information Criterion (AIC) of hmm
#' and observation.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations (may be NULL if \code{llk} is provided
#' @param np number of free parameters of hmm density function (e.g. 2 for normal, 1 for poisson).
#' @param nt number of free parameters for transition matrix.
#' @param ni number of free parameters for initial probabilities.
#' @param k the penalty parameter. the default k=2 is the classical AIC.
#' @param llk numeric, log-likelihood(obs | hmm). if NA then llk will be computed using \link{thmm.forward}.
#' @param .useC logical if TRUE (default) use C code else use R code for \link{thmm.forward}
#' @return numeric AIC
#' @details
#' AIC = \code{-2 * llk + (nt + ni + M * np) * k}\cr
#' with \code{M=number of states}
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- thmm.simulate(hmm, 100, .seed=0)$values
#' thmm.aic(hmm, obs, np=2, nt=1, ni=0)
#'
thmm.aic <- function(hmm, obs, np,
                     nt=nrow(hmm$trans)*(nrow(hmm$trans)-1),
                     ni=nrow(hmm$trans)-1,
                     k = 2,
                     llk=NA,
                     .useC=TRUE) {
  if (is.na(llk))
    llk <- thmm.forward(hmm, obs, .useC=.useC)$loglike
  npar <- nt + ni + nrow(hmm$trans) * np
  -2 * llk + k * npar
}

# -------------------------------------------------
#' compute BIC
#' @description compute Bayesian Information Criterion (BIC) of hmm
#' and observation.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param np number of free parameters of hmm density function (e.g. 2 for normal, 1 for poisson).
#' @param nt number of free parameters for transition matrix.
#' @param ni number of free parameters for initial probabilities.
#' @param llk numeric, log-likelihood(obs | hmm). if NA then llk will be computed using \link{thmm.forward}.
#' @param .useC logical if TRUE (default) use C code else use R code for \link{thmm.forward}
#' @return numeric BIC
#' @details
#' BIC = \code{-2 * llk + (nt + ni + M * np) * log(N)}\cr with
#' \code{M=number of states; N=length(obs)}
#' @examples
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- thmm.simulate(hmm, 100, .seed=0)$values
#' thmm.bic(hmm, obs, np=2, nt=1, ni=0)
#'
thmm.bic <- function(hmm, obs, np,
                     nt=nrow(hmm$trans)*(nrow(hmm$trans)-1),
                     ni=nrow(hmm$trans)-1,
                     llk=NA,
                     .useC=TRUE) {
  thmm.aic(hmm, obs, np=np, nt=nt, ni=ni, k=log(length(obs)), llk=llk, .useC=.useC)
}

