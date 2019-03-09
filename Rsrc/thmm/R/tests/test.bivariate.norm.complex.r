#
# additional manual tests : bivariate normal distribution
# (using complex number implementation)
# see also : test.multivar.norm.r for a more general multivariate
#            version
#

if (FALSE) {

  require(thmm)

  .c <- function(x,y=x) complex(real=x, imaginary=y)

  #
  # distribution: bivariate normal (with 0 covariance)
  # implementation using complex numbers
  #
  dxynorm <- function(x, mean=.c(0), sd=.c(1), log=FALSE) {
    re <- dnorm(Re(x), mean=Re(mean), sd=Re(sd), log=log)
    im <- dnorm(Im(x), mean=Im(mean), sd=Im(sd), log=log)
    if (log) re+im else re*im
  }

  #
  # random generation function
  #
  rxynorm <- function(n, mean=.c(0), sd=.c(1)) {
    re <- rnorm(n, mean=Re(mean), sd=Re(sd))
    im <- rnorm(n, mean=Im(mean), sd=Im(sd))
    .c(re, im)
  }

  hmm <- thmm.init(dxynorm, 0.1,
                   mean=c(.c(10, 20), .c(50, 100)),
                   sd=c(.c(5,10),.c(25, 40)))

  x <- thmm.simulate(hmm, 100, with.values=T)

  v <- thmm.viterbi(hmm, x$values, .useC=T)

  lvl <- thmm.parameters(hmm, "mean")

  par(mfrow=c(2,1))
  plot(Re(x$values))
  lines(Re(lvl[x$states]), col=3, lwd=2)
  lines(Re(lvl[v$states]), col=2, lty=2, lwd=2)
  plot(Im(x$values))
  lines(Im(lvl[x$states]), col=3, lwd=2)
  lines(Im(lvl[v$states]), col=2, lty=2, lwd=2)
  par(mfrow=c(1,1))

}

