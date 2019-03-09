# -------------------------------------------------
# $Id: thmm.r 396 2019-01-02 22:53:10Z viari $
# tinyhmm : package entry (documentation and attachment)

# -------------------------------------------------
#' Overview of package thmm
#' @name thmm
#' @description
#' A tiny R package for discrete time hidden Markov models (DTHmm)
#' with careful handling of underflow.\cr
#' Strongly inspired by the 'HiddenMarkov' package from David Harte (\url{https://cran.r-project.org/web/packages/HiddenMarkov/index.html})
#' but numerically more stable (for forward/backward computations) and with different (hopefully simpler) API.
#' (the 'HiddenMarkov' package has more features (including Markov modulated GLMs))
#' \subsection{Concepts}{
#' A discrete time hidden Markov model is defined by:\cr
#' \itemize{
#' \item m (discrete) states
#' \item a probability density function \code{density} describing the (univariate
#' or multivariate, continuous or discrete) distribution with different parameters
#' for each state.
#' \item a transition probability matrix (of size m x m) between states
#' \item a vector (of length m) of initial probability for each state
#' }
#' \bold{important note}: for the sake of simplicty the remaining of this
#' documentation as well as functions documentation, will assume that
#' \code{density} is univariate. However the library can be used with multivariate
#' densities as well (of course this comes at a price of a bit of complication).
#' Please consult the \bold{multivariate densities} section below.
#'
#' Given a sequence of observed (univariate) random variable \code{X}
#' noted \code{obs=obs_1, ..., obs_n} this package solves the following problems:\cr
#' \enumerate{
#' \item Given the model parameters, compute the probability of \code{obs}.
#' This problem is solved by the \bold{forward and backward} algorithms.
#' \item Given the model parameters, find the most likely sequence of (hidden) states
#' which could have generated \code{obs}. This problem is solved by the \bold{Viterbi}
#' algorithm.
#' \item Given \code{obs}, find the most likely set of \code{density} parameters and
#' transition probabilities. This problem is solved by the \bold{Baum-Welch} algorithm.
#' }
#' }
#' \subsection{forward algorithm}{
#' is used to compute:
#' \itemize{
#' \item\code{alpha_{i,j} = Pr{ X_1 = obs_1, ..., X_i = obs_i, state_i = j | hmm}}
#' that is the probability of seeing the partial sequence
#' \code{(obs_1, ..., obs_i)} and ending up in state j at time i for this
#' hmm.
#' }
#' from which one can derive the \bold{log-likelihood} of observation \code{obs} with this hmm:
#' \itemize{
#' \item\code{log(Pr{ X_1 = obs_1, ..., X_n = obs_n | hmm})}
#' }
#' }
#' \subsection{backward algorithm}{
#' is used to compute:
#' \itemize{
#' \item\code{beta_{i,j} = Pr{ X_{i+1} = obs_{i+1}, ..., X_n = obs_n | state_i = j , hmm}}
#' that is the probability of the ending partial sequence
#' \code{(obs_i+1, ..., obs_n)} given that we started at state j at time i,
#' for this hmm
#' }
#' }
#' \subsection{forward-backward algorithm}{
#' putting everything together, one can further compute:
#' \itemize{
#' \item\code{gamma_{i,j} = Pr{ state_i = j | obs , hmm}}
#' that is the probability of being at time i in state j given
#' this observation and this hmm
#' \item\code{rho_{i} = Pr{ X_1 = obs_1, ..., X_i = obs_i | hmm}}
#' that is the probability of seeing the partial sequence
#' \code{(obs_1, ..., obs_i)} with this hmm
#' }
#' }
#' \subsection{Viterbi's algorithm}{
#' The purpose of the Viterbi's algorithm is to determine
#' the sequence of states \code{(k_1*, ..., k_n*)} which maximises
#' the joint distribution of the hidden states given the entire
#' observed process. i.e:
#' \itemize{
#' \item\code{(k_1*, ..., k_n*) = argmax(Pr(state_1=k_1, ..., state_n=k_n, X_1=obs_1, ..., X_n=obs_n | hmm))}
#' }
#' this also allows to compute:
#' \itemize{
#' \item\code{nu_{i+1,j} = Pr(state_1=k_1*, ..., state_i=k_i*, state_i+1 = j,  X_1=obs_1, ..., X_i=obs_i | hmm)}
#' where k_i* is the optimal state at time i. (local decoding)
#' \item\code{viterbi_score = Pr(state_1=k_1*, ..., state_n=k_n*, obs | hmm)} the joint
#' probability of optimal sequence of states and observation.
#' \item\code{probseq  = Pr(state_1=k_1*, ..., state_n=k_n* | obs, hmm)} the probability
#' of the optimal sequence of state conditionally to the observation.
#' }
#' }
#' \subsection{Baum-Welch algorithm}{
#' The purpose of the Baum-Welch algorithm is to estimate the HMM parameters
#' (i.e. distribution parameters and transition probabilities) that
#' best fit a given observation. This is a version of the EM algorithm that
#' iteratively alternates two steps: the \bold{E-step}
#' and the \bold{M-step}. The E-step is generic but the M-step depends upon
#' each distribution. A M-step for Normal distribution is provided
#' (as \code{mstep.dnorm}) and some others are given in examples.
#' See \link{thmm.baumwelch} for more details (in particular how to
#' control which parameters are actually adjusted).
#' }
#' \subsection{Implementation}{
#' current implementation uses either pure R code or compiled C code.
#' the default is to use (quicker) C code, R code has been kept for
#' debugging purposes.
#' }
#' \subsection{Multivariate densities}{
#' Instead of a univariate density function you may define a bi- or multi-
#' variate density function as well.
#' The main difference is that instead of a numerical vector of observations,
#' you should provide a list of tuples (each element in the tuple corresponds
#' to one of the random variable). For the bivariate case, however, a trick is
#' to use complex numbers instead of a list of doublets. This makes the definition
#' of density and observation much simpler. Please see examples
#' below as well as samples in the \code{test} directory.
#' }
#' \subsection{Serializing}{
#' \bold{Important note} In the current implementation a \code{DTHmm} object
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
#' }
#'
#' @examples
#' # ---------------------------------------
#' # Normal (gaussian) HMM with two states
#' # state 1 : mean=-1, sd=0.1
#' # state 2 : mean=+1, sd=0.1
#' # ergodic HMM with transition probability = 0.1
#' #
#' # full specification:
#' trans <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow=2)
#' init <- c(0.5, 0.5)
#' hmm <- thmm.init(dnorm, trans, init, mean=c(-1,1), sd=c(0.1, 0.1))
#' #
#' # *same as previous* with simplified call:
#' hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
#'
#' # simulate some sample
#' obs <- thmm.simulate(hmm, 100, .seed=0)
#'
#' # run viterbi to retrieve states
#' vit <- thmm.viterbi(hmm, obs$values)
#'
#' \dontrun{
#' # compare actual and predicted states
#' plot(obs$values)
#' lines(thmm.parameters(hmm, "mean")[obs$states], col=3, lwd=5)
#' lines(thmm.parameters(hmm, "mean")[vit$states], col=2)}
#'
#' # ---------------------------------------
#' # poisson distribution
#' #
#' hmm <- thmm.init(dpois, 0.1, lambda=c(5, 10))
#' obs <- thmm.simulate(hmm, 100, .seed=0)
#' vit <- thmm.viterbi(hmm, obs$values)
#' \dontrun{
#' plot(obs$values)
#' lines(thmm.parameters(hmm, "lambda")[obs$states], col=3, lwd=5)
#' lines(thmm.parameters(hmm, "lambda")[vit$states], col=2)}
#'
#' # ---------------------------------------
#' # mixture of gaussians
#' #
#' # we need to define the pdf
#' #
#' # note that this function is 'vectorized' on parameters (and x).
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
#' # and random generator
#' # again 'vectorized' on parameters (and x).
#' rmixnorm <- function(n, mean=0, sd=1, prob=1) {
#'   .cycle <- function(x, n) head(rep(x, n), n)
#'   msp <- mapply(function(m, s, p) list(mean=m, sd=s, prob=p), mean, sd, prob, SIMPLIFY=FALSE)
#'   unlist(lapply(seq_len(n), function(i) sapply(msp, function(p) {
#'          r <- mapply(function(m, s) rnorm(1, m, s), p$mean, p$sd)
#'          sample(r, size=1, prob=.cycle(p$prob, length(r)))
#'   })))
#' }
#'
#' # hmm with 2 states
#' # state1: mixture of 2 gaussians with mean=-1,1 sd=0.1,0.1 and prob=0.1,0.9
#' # state2: single gaussian mean=0 sd=0.2
#' hmm <- thmm.init(dmixnorm, 0.2, mean=list(c(-1,1), 0), sd=list(0.1, 0.2),
#'                                 prob=list(c(0.5,0.5), 1))
#' obs <- thmm.simulate(hmm, n=100, .seed=0)
#' vit <- thmm.viterbi(hmm, obs$values)
#' \dontrun{
#' plot(obs$values)
#' lines((2-obs$states), col=3)
#' y <- thmm.parameters(hmm, "mean")[vit$states]
#' lines(sapply(y, function(x) x[2%%length(x)+1]), col=2)}
#'
#' # ---------------------------------------
#' # discrete distribution : the dishonest casino
#' #
#' # the pdf (with p6 parameter = probability of drawing a six)
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
#' # and the random generator (d->r)ice
#' rice <- function(n, p6=1/6) {
#'   sample(1:6, n, prob=c(rep((1-p6)/5, 5), p6), replace=TRUE)
#' }
#'
#' hmm <- thmm.init(dice, 0.1, p6=c(1/6, 3/6))  # the second dice is loaded
#' obs <- thmm.simulate(hmm, 100, .seed=0)
#' vit <- thmm.viterbi(hmm, obs$values)
#' \dontrun{
#' plot(obs$values)
#' lines((obs$states-1)*5+1, col=3) # truth
#' lines((vit$states-1)*5+1, col=2) #predicted}
#'
#' # ---------------------------------------
#' # univariate normal : Baum-Welch
#' #
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
#' ctrl <- thmm.bw.ctrl(hmm0, do.trans=FALSE, do.init=FALSE)
#' bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
#'
#'
#' # ---------------------------------------
#' # bivariate normal
#' # implemented using complex numbers
#' # see test.multivar.norm.r for a more general multivariate version
#' #
#'
#' # helper
#' .c <- function(x,y=x) complex(real=x, imaginary=y)
#'
#' #
#' # pdf bivariate normal (with 0 covariance)
#' #
#' dxynorm <- function(x, mean=.c(0), sd=.c(1), log=FALSE) {
#'   re <- dnorm(Re(x), mean=Re(mean), sd=Re(sd), log=log)
#'   im <- dnorm(Im(x), mean=Im(mean), sd=Im(sd), log=log)
#'   if (log) re+im else re*im
#' }
#'
#' #
#' # random generation function
#' #
#' rxynorm <- function(n, mean=.c(0), sd=.c(1)) {
#'   re <- rnorm(n, mean=Re(mean), sd=Re(sd))
#'   im <- rnorm(n, mean=Im(mean), sd=Im(sd))
#'   .c(re, im)
#' }
#'
#' hmm <- thmm.init(dxynorm, 0.1,
#'                  mean=c(.c(10, 20), .c(50, 100)),
#'                  sd=c(.c(5,10),.c(25, 40)))
#'
#' obs <- thmm.simulate(hmm, 100, with.values=TRUE)
#' vit <- thmm.viterbi(hmm, obs$values)
#' lvl <- thmm.parameters(hmm, "mean")
#'
#' \dontrun{
#' par(mfrow=c(2,1))
#' plot(Re(obs$values))
#' lines(Re(lvl[obs$states]), col=3, lwd=2)
#' lines(Re(lvl[vit$states]), col=2, lty=2, lwd=2)
#' plot(Im(obs$values))
#' lines(Im(lvl[obs$states]), col=3, lwd=2)
#' lines(Im(lvl[vit$states]), col=2, lty=2, lwd=2)
#' par(mfrow=c(1,1))}
#'
#' # ---------------------------------------
#' # bivariate normal + poisson
#' #
#'
#' # helper : take ith element of tuple
#' .i <- function(x, i) sapply(x, function(x) x[i])
#'
#' # pdf : bivariate normal + poisson
#' #
#'
#' dnorpois <- function(x, mean=0, sd=1, lambda=10, log=FALSE) {
#'   .prod <- if (log) sum else prod
#'   unlist(mapply(function(m, s, l) {
#'     xn <- dnorm(.i(x,1), m, s, log=log)
#'     xp <- dpois(.i(x,2), l, log=log)
#'     mapply(.prod, xn, xp)
#'   },
#'   mean, sd, lambda, SIMPLIFY=FALSE), use.names=FALSE)
#' }
#'
#' # random number generator
#' #
#'
#' rnorpois <- function(n, mean=0, sd=1, lambda=10) {
#'   mapply(c, rnorm(n, mean, sd), rpois(n, lambda), SIMPLIFY=FALSE)
#' }
#'
#' hmm <- thmm.init(dnorpois, 0.1,
#'                  mean=c(10, 20, 30), sd=c(1, 2, 3),
#'                  lambda=c(5, 10, 15))
#'
#' obs <- thmm.simulate(hmm, 100, with.values=TRUE)
#'
#' vit <- thmm.viterbi(hmm, obs$values)
#'
#' lvl.mean <- thmm.parameters(hmm, "mean")
#' lvl.lamb <- thmm.parameters(hmm, "lambda")
#'
#' \dontrun{
#' par(mfrow=c(2,1))
#' plot(.i(obs$values,1), main="normal")
#' lines(lvl.mean[obs$states], col=3, lwd=2)
#' lines(lvl.mean[vit$states], col=2, lty=2, lwd=2)
#' plot(.i(obs$values,2), main="poisson")
#' lines(lvl.lamb[obs$states], col=3, lwd=2)
#' lines(lvl.lamb[vit$states], col=2, lty=2, lwd=2)
#' par(mfrow=c(1,1))}
#'
#'
NULL

# =================================================
# package attachment
# =================================================
#

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('+ attaching ', pkgname)
}
