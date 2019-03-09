#
# additional manual tests : binomial distribution
#

if (FALSE) {

  require(thmm)

  #
  # discrete distribution: binomial
  #
  hmm <- thmm.init(dbinom, 0.1, size=c(50, 100), prob=c(0.5, 0.1))
  x <- thmm.simulate(hmm, 100, with.values=T)
  v <- thmm.viterbi(hmm, x$values, .useC=T)
  lvl <- thmm.parameters(hmm, "size") * thmm.parameters(hmm, "prob")

  plot(x$values)
  lines(lvl[x$states], col=3, lwd=2)
  lines(lvl[v$states], col=2, lty=2, lwd=2)
}

