# -------------------------------------------------
# $Id: thmm.baumwelch.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : baum-welch algorithm
#

# -------------------------------------------------
#' E-Step of EM Baum-Welch algorithm
#' @description performs the expectation step of the EM algorithm
#' for a discrete time HMM process.
#' this function is called by the \link{thmm.baumwelch}.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param obs numerical vector of observations
#' @param .useC logical if TRUE (default) use C code else use R code
#' @return list with three components:\cr
#' \itemize{
#' \item u length(obs)*number_states matrix containing estimates of the
#' conditional expectations (see details)
#' \item v length(obs)*number_states*number_states matrix containing estimates of the
#' conditional expectations (see details)
#' \item loglike log-likelihood
#' }
#' @details
#' u is defined as:\cr
#' \code{u{i,j}=Pr(state_i=j | obs, hmm)}\cr
#' that is the same as \code{gamma} in \link{thmm.forward.backward}
#'
#' v is defined as:\cr
#' \code{v({i,j,k}=Pr(state_i-1=j, state_i=k | obs, hmm)}\cr
#' also known as \code{chsi}
#'
#' @seealso \link{thmm.baumwelch}
#'
thmm.estep <- function (hmm, obs, .useC=TRUE) {
  n <- length(obs)
  m <- nrow(hmm$trans)

  fwb <- thmm.forward.backward(hmm, obs, .useC=.useC)

  u <- fwb$gamma

  v <- array(NA, dim=c(n-1, m, m))

  logdens <- .logdens(hmm, obs)

  for (k in seq_len(m)) {
    logprob  <- logdens[-1, k]
    logtrans <- matrix(log(hmm$tran[, k]), byrow=T, nrow=n-1, ncol=m)
    logpbeta <- matrix(logprob + fwb$logbeta[-1, k], byrow=F, nrow=n-1, ncol=m)
    v[,,k] <- exp(logtrans + fwb$logalpha[-n, ] + logpbeta - fwb$loglike)
  }

  list(u=u, v=v, loglike=fwb$loglike)
}

# -------------------------------------------------
#' control parameters for Baum-Welch algorithm
#' @description make a list of parameters to control
#' \link{thmm.baumwelch} execution.
#' @param hmm DTHmm model (from \link{thmm.init})
#' @param maxiter integer max number of iteration (NA will set to hmm
#' dependent value)
#' @param tol numeric convergence criterion (delta_log-likelihood)
#' @param verbose logical be verbose
#' @param converge expression of the convergence criterion
#' @param do.trans logical do optimize transition matrix
#' @param do.init logical do optimize initial probabilities
#' @param constraint if not NULL, should contain a function receiving a list of
#' optimized parameters (at each M-step) and should return a list (with
#' the same format) of constrained parameters values (see examples).
#' @param ... any parameter passed to \code{<density>.mstep} function
#' (see \link{thmm.baumwelch}). in particular:
#' \itemize{
#' \item parameters of the form
#' \code{do.<parameter>} are interpreted as logical controling whether
#' or not we should optimize the <parameter>.
#' note that to prevent update you should explicitely set \code{do.<parameter>}
#' to \code{FALSE}. if \code{do.<parameter>} is absent (or set to TRUE) then
#' \code{<parameter>} \bold{will be} updated.
#' \item parameters of the form \code{upper|lower.<parameter>}
#' are interpreted as numerical upper/lower bounds. if absent then
#' no bounds (i.e. [-Inf, +Inf]) are assumed.
#' }
#' @return named list of control parameters for \link{thmm.baumwelch}
#' @note \code{constraint} provides a more general form of control than
#' \code{do.<parameter>, upper|lower.<parameter>} but may be less efficient
#' since it is called right after the mstep function.
#' @note (for developpers) \code{do.<parameter>, upper|lower.<parameter>}
#' should be implemented for every mstep function. see \link{mstep.dnorm} as an exemple.
#' \code{constraint} is called by \link{thmm.baumwelch} directly.
#' @seealso \link{thmm.baumwelch}
#'
thmm.bw.ctrl <- function(hmm, maxiter=NA, tol=1e-5, verbose=FALSE,
                         converge=expression(difLL <= ctrl$tol),
                         do.trans=TRUE, do.init=TRUE, constraint=NULL,
                         ...) {
  if (is.na(maxiter)) maxiter <- max(500, nrow(hmm$trans)*100)
  c(list(maxiter=maxiter, tol=tol, verbose=verbose, converge=converge,
         do.trans=do.trans, do.init=do.init, constraint=constraint), list(...))
}

# -------------------------------------------------
#' Baum-Welch algorithm
#' @description HMM parameters estimation using the Baum-Welch algorithm
#' @param hmm0 a starting DTHmm (see \link{thmm.init})
#' @param obs numerical vector of observations
#' @param ctrl a list of control settings obtained by \link{thmm.bw.ctrl}
#' @param .mstep function name of the mstep function (see details)
#' @param .useC logical if TRUE (default) use C code else use R code
#'
#' @return a named list of three elements:
#' \itemize{
#' \item hmm : optimized hmm
#' \item info : information about the iterative process
#' \item loglike : final log-likelihood
#' }
#' @details
#' the Baum-Welch EM algorithm iteratively alternates two steps: the \bold{E-step}
#' and the \bold{M-step}. The \bold{E-step} (expectation) is distribution independent
#' and is performed by the \link{thmm.estep} function. The \bold{M-step} (maximisation)
#' is distribution dependent and you should provide a distribution specific function
#' to perform it. The name of this function is \code{mstep.<density>}
#' (e.g. \code{mstep.dnorm} for a gaussian distribution) unless you provide another
#' name as the \code{.mstep} parameter.
#' Default function is provided for \code{dnorm} (may
#' be extended in the future). Have a look at \link{mstep.dnorm} for an example
#' of function format.\cr
#' you may control which hmm parameters are actually optimized with
#' \code{ctrl}. see \link{thmm.bw.ctrl}.
#' @examples
#' # reference hmm
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#' obs <- thmm.simulate(hmm, n=1000, .seed=0)
#'
#' # this one converge to the correct solution
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,5))
#' bw   <- thmm.baumwelch(hmm0, obs$values)
#'
#' # but not that one
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,10))
#' bw   <- thmm.baumwelch(hmm0, obs$values)
#'
#' # so adding constraints: no update of trans nor init
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,10))
#' ctrl <- thmm.bw.ctrl(hmm0, do.trans=FALSE, do.init=FALSE, verbose=TRUE)
#' bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
#'
#' # that one do not converge either to proper solution
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
#' bw   <- thmm.baumwelch(hmm0, obs$values)
#'
#' # adding constraints: mean bounds
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
#' ctrl <- thmm.bw.ctrl(hmm0, lower.mean=c(-2,0), upper.mean=c(0,2), verbose=TRUE)
#' bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
#'
#' # adding constraint: as as previous but more general form
#' hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
#' myconst <- function(params) {
#'   params$mean <- pmax(pmin(params$mean, c(0,2)), c(-2,0))
#'   params
#' }
#' ctrl <- thmm.bw.ctrl(hmm0, constraint=myconst, verbose=TRUE)
#' bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
#'
thmm.baumwelch <- function(hmm0, obs, ctrl=thmm.bw.ctrl(hmm0), .mstep, .useC=TRUE) {

  .def  <- function(sym) (is.null(sym)) || sym
  .isna <- function(x) any(is.na(x))
  .nona <- function(x) ! .isna(x)

  # backport of get0 for R < 3.2
  if (! exists("get0", mode="function"))
   get0 <- function(x, envir, mode="any", inherits=TRUE, ifnotfound=NULL) {
       mget(x[1L], envir=envir, mode=mode, inherits=inherits,
            ifnotfound=list(ifnotfound))[[1L]]
     }

  n <- length(obs)
  m <- nrow(hmm0$trans)

  mstep.name <- if (missing(.mstep)) paste0("mstep.", hmm0$info$density) else .mstep
  mstep.env <- parent.frame()
  mstep.fun <- get0(mstep.name, envir=mstep.env, mode="function")

  if (is.null(mstep.fun))
    stop(mstep.name, " : mstep function not found")

  hmm <- hmm0
  oldLL <- newLL <- -Inf

  for (iter in seq_len(ctrl$maxiter)) {

    # ----------
    # E-step
    #
    cond <- thmm.estep(hmm, obs, .useC=.useC)
    newLL <- cond$loglike
    difLL <- newLL - oldLL

    if (ctrl$verbose)
      .out("iter=", iter, " LL=", newLL, " diff=", difLL)

    # check convergence
    #

    # NaN
    if (.isna(difLL))
      stop("NaN encountered during LL computation (try to change initialization)")

    # negative difLL
    if (difLL < 0) {
      trueNeg <- abs(difLL) > ctrl$tol  # true negative difLL or almost 0 ?
      if (trueNeg)
        warning("Worse log-likelihood on last iteration")
      return(list(hmm=hmm0, info=list(conv=!trueNeg, iter=iter, loglike=oldLL)))
    }

    # convergence achieved
    if (eval(ctrl$converge)) break

    # ----------
    # M-step
    #
    if (.def(ctrl$do.trans) && (m > 1)) {
      trans <- diag(1/apply(cond$v, MARGIN=2, sum)) %*% apply(cond$v, MARGIN=c(2,3), sum)
      if (.nona(trans)) hmm$trans <- trans
    }

    if (.def(ctrl$do.init) && (m > 1)) {
      init <- .marginprob(hmm$trans)
      if (.nona(init)) hmm$init <- init
    }

    params <- do.call(mstep.name, c(list(obs), list(cond), list(ctrl),
                                    unlist(hmm$info$args, recursive=F)),
                      envir=mstep.env)

    if (is.function(ctrl$constraint))
      params <- ctrl$constraint(params)

    # built next hmm
    #
    hmm0 <- hmm
    hmm <- do.call(thmm.init, c(list(hmm$info$density), list(hmm$trans), list(hmm$init), params))

    oldLL <- newLL
  }

  list(hmm=hmm, info=list(conv=(iter<ctrl$maxiter), iter=iter, loglike=newLL))
}

