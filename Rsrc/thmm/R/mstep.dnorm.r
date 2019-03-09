# -------------------------------------------------
# $Id: mstep.dnorm.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : M-step for univariate normal distribution

# -------------------------------------------------
#' M-step function for univariate normal distribution
#' @description used as the M-step function of \link{thmm.baumwelch} for
#' univariate normal distribution.
#' @param obs numerical vector of observations
#' @param cond estimates of the conditional expectations as computed by \link{thmm.estep}
#' @param ctrl list of control parameters created by \link{thmm.bw.ctrl}
#' @param mean list of states means (may be missing)
#' @param sd list of states sd (may be missing)
#' @return list of updated parameters mean and sd
#' @seealso \link{thmm.baumwelch}
#'

mstep.dnorm <- function(obs, cond, ctrl, mean, sd) {
  .deflog <- function(sym) (is.null(sym)) || sym
  .defnum <- function(sym) (length(sym) != 0)
  .unlist <- function(x) unlist(x, use.names=F)

  n <- length(obs)
  m <- ncol(cond$u)

  cscu <- .colSums(cond$u)

  if (.deflog(ctrl$do.mean)) {
    nmean <- (matrix(obs, nrow=1) %*% cond$u) / cscu
    lmean <- list(mean=nmean)
  }
  else if (missing(mean)) {
    nmean <- 0 # this is dnorm default, case of an hmm with mean=0 state(s)
    lmean <- list()
  }
  else {
    nmean <- mean
    lmean <- list(mean=nmean)
  }

  if (.defnum(ctrl$lower.mean) && (length(lmean) > 0))
    lmean <- list(mean=pmax(.unlist(lmean), ctrl$lower.mean))
  if (.defnum(ctrl$upper.mean) && (length(lmean) > 0))
    lmean <- list(mean=pmin(.unlist(lmean), ctrl$upper.mean))

  if (.deflog(ctrl$do.sd)) {
    var <- (matrix(obs, nrow=n, ncol=m) - matrix(nmean, nrow=n, ncol=m, byrow=T))^2 * cond$u
    var <- pmax(.colSums(var) / cscu, .Machine$double.eps)
    lsd <- list(sd=sqrt(var))
  } else if (missing(sd))
    lsd <- list()
  else
    lsd <- list(sd=sd)

  if (.defnum(ctrl$lower.sd) && (length(lsd) > 0))
    lsd <- list(sd=pmax(.unlist(lsd), ctrl$lower.sd))
  if (.defnum(ctrl$upper.sd)  && (length(lsd) > 0))
    lsd <- list(sd=pmin(.unlist(lsd), ctrl$upper.sd))

  c(lmean, lsd)
}
