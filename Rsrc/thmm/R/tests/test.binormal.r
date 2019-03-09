#
# additional manual tests : binormal (with baum-welch)
#

if (FALSE) {

  require(thmm)

  #
  # binormal density function
  #
  dbinorm <- function(x, mean=1, sd=1, log=FALSE) {
    ms <- mapply(function(m, s) list(mean=m, sd=s), mean, sd, SIMPLIFY=F)
    res <- if (log) {
      lapply(x, function(x) sapply(ms, function(p) {
        pp <- c(dnorm(x, p$mean, p$sd, log=T), dnorm(x, -p$mean, p$sd, log=T))
        mp <- max(pp)
        mp + base::log(sum(exp(pp-mp)))
      }))
    } else {
      lapply(x, function(x) sapply(ms, function(p)
        dnorm(x, p$mean, p$sd, log=F) + dnorm(x, -p$mean, p$sd, log=F)))
    }
    unlist(res)
  }

  #
  # binormal random generation function
  #
  rbinorm <- function(n, mean=1, sd=1) {
    .cycle <- function(x, k) head(rep(x, k), k)
    m <- max(n, length(mean), length(sd))
    mean <- .cycle(mean, m) * sample(c(-1,1), m, replace=T)
    rnorm(n, mean=mean, sd=sd)
  }

  #
  # binormal Mstep
  #
  mstep.dbinorm <- function(obs, cond, ctrl, mean=1, sd=1) {
    .def <- function(sym) (is.null(sym)) || sym
    .colSums <- function(mat) rowSums(t(mat))

    n <- length(obs)
    m <- ncol(cond$u)

    if (.def(ctrl$do.mean)) {
      mu <- matrix(sapply(c(-1,1), function(sgn) {
        ccu <- cond$u * mapply(function(m,s) dnorm(obs, sgn*m, s), mean, sd)
        (matrix(obs, nrow=1) %*% ccu) / .colSums(ccu)
      }), nrow=m, ncol=2)
      nmean <- (mu[,2]-mu[,1])/2
      lmean <- list(mean=nmean)
    }
    else {
      nmean = mean
      lmean <- list(mean=mean)
    }

    if (.def(ctrl$do.sd)) {
      mobs <- matrix(obs, nrow=n, ncol=m)
      pvar <- (mobs - matrix(nmean, nrow=n, ncol=m, byrow=T))^2
      mvar <- (mobs - matrix(-nmean, nrow=n, ncol=m, byrow=T))^2
      var <- pmin(pvar, mvar) * cond$u
      var <- pmax(.colSums(var) / .colSums(cond$u), .Machine$double.eps)
      lsd   <- list(sd=sqrt(var))
    } else if (missing(sd))
      lsd <- list()
    else
      lsd <- list(sd=sd)

    c(lmean, lsd)
  }

  ##
  ## test
  ##

  hmm <- thmm.init(dbinorm, 0.1, mean=c(0, 2), sd=c(0.1, 0.1))
  set.seed(1)
  obs <- thmm.simulate(hmm, 100)

  plot(obs$values)
  lines(2*(obs$states-1), col=3, lwd=2)

  hmm0 <- thmm.init(dbinorm, 0.1, mean=c(0.05, 5), sd=c(1,1))
  vt0 <- thmm.viterbi(hmm0, obs$values)
  lines(2*(1-vt0$states), col=4)

  ctrl <- thmm.bw.ctrl(hmm0, verbose=T, do.trans=F, do.init=F)
  bw <- thmm.baumwelch(hmm0, obs$values, ctrl, .useC=T)
  vt <- thmm.viterbi(bw$hmm, obs$values)
  lines(2*(1-vt$states), col=2, lty=2, lwd=2)
}
