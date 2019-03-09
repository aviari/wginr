#
# additional manual tests : multivariate normal distribution
# see also : test.bivariate.norm.complex.r for a bivariate
#            version implemented with complex numbers.
#

if (FALSE) {

  require(thmm)

  #
  # distribution: multivariate normal (with 0 covariance)
  # k = 3
  #

  dmvnorm <- function(x, mean=list(c(0)), sd=list(c(1)), log=FALSE) {
    .prod <- if (log) sum else prod
    unlist(mapply(function(mm, ss)
             sapply(lapply(x, function(xx)
                      mapply(function(m,s) .prod(dnorm(xx, m, s, log=log)),
                      mm, ss)),
                    .prod),
           mean, sd, SIMPLIFY=F), use.names=F)
  }

  #
  # random generation function
  #

  rmvnorm <- function(n, mean=list(c(0)), sd=list(c(1))) {
    unlist(mapply(function(mm, ss) {
              y <- mapply(function(m,s) rnorm(n, m, s),
                   mm, ss, SIMPLIFY=F)
              do.call(mapply, c(FUN=c, y, SIMPLIFY=F))
              },
    mean, sd, SIMPLIFY=F), recursive=F)
  }

  hmm <- thmm.init(dmvnorm, 0.1,
                   mean=list(c(10, 20, 30), c(50, 100, 150)),
                   sd=list(c(2, 4, 6), c(6, 10, 15)))

  x <- thmm.simulate(hmm, 100, with.values=T)

  v <- thmm.viterbi(hmm, x$values, .useC=T)

  lvl <- thmm.parameters(hmm, "mean")

  .i <- function(x, i) sapply(x, function(x) x[i])

  par(mfrow=c(3,1))
  plot(.i(x$values,1))
  lines(.i(lvl[x$states], 1), col=3, lwd=2)
  lines(.i(lvl[v$states], 1), col=2, lty=2, lwd=2)
  plot(.i(x$values,2))
  lines(.i(lvl[x$states], 2), col=3, lwd=2)
  lines(.i(lvl[v$states], 2), col=2, lty=2, lwd=2)
  plot(.i(x$values,3))
  lines(.i(lvl[x$states], 3), col=3, lwd=2)
  lines(.i(lvl[v$states], 3), col=2, lty=2, lwd=2)
  par(mfrow=c(1,1))

}

