#
# additional manual tests : bivariate distribution : normal & poisson
#

if (FALSE) {

  require(thmm)

  #
  # distribution: bivariate normal + poisson (with 0 covariance)
  #

  dnorpois <- function(x, mean=0, sd=1, lambda=10, log=FALSE) {
    .i <- function(x, i) sapply(x, function(x) x[i])
    .prod <- if (log) sum else prod
    unlist(mapply(function(m, s, l) {
              xn <- dnorm(.i(x,1), m, s, log=log)
              xp <- dpois(.i(x,2), l, log=log)
              mapply(.prod, xn, xp)
           },
           mean, sd, lambda, SIMPLIFY=F), use.names=F)
  }

  rnorpois <- function(n, mean=0, sd=1, lambda=10) {
    mapply(c, rnorm(n, mean, sd), rpois(n, lambda), SIMPLIFY=F)
  }

  hmm <- thmm.init(dnorpois, 0.1,
                   mean=c(10, 20, 30), sd=c(1, 2, 3),
                   lambda=c(5, 10, 15))

  x <- thmm.simulate(hmm, 100, with.values=T)

  v <- thmm.viterbi(hmm, x$values, .useC=T)

  lvl.mean <- thmm.parameters(hmm, "mean")
  lvl.lamb <- thmm.parameters(hmm, "lambda")

  par(mfrow=c(2,1))
  plot(.i(x$values,1), main="normal")
  lines(lvl.mean[x$states], col=3, lwd=2)
  lines(lvl.mean[v$states], col=2, lty=2, lwd=2)
  plot(.i(x$values,2), main="poisson")
  lines(lvl.lamb[x$states], col=3, lwd=2)
  lines(lvl.lamb[v$states], col=2, lty=2, lwd=2)
  par(mfrow=c(1,1))

}

