#
# additional manual tests : baum-welch convergence
#

if (FALSE) {

  compare <- function(h1, h2) {
    .comp <- function(what="mean")
      sum(abs(thmm.parameters(h1, what) - thmm.parameters(h2, what)))
    .comp("mean") * .comp("sd")
  }

  # reference hmm (i.e. solution to be found)
  hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
  obs <- thmm.simulate(hmm, n=1000, .seed=0)

  # this one converge to the correct solution
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,5))
  bw   <- thmm.baumwelch(hmm0, obs$values)
  compare(hmm, bw$hmm)

  # but not that one
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,10))
  bw   <- thmm.baumwelch(hmm0, obs$values)
  compare(hmm, bw$hmm)

  # so adding constraints: no update of trans nor init
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,10))
  ctrl <- thmm.bw.ctrl(hmm0, do.trans=F, do.init=F, verbose=T)
  bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
  compare(hmm, bw$hmm)

  # that one do not converge either to proper solution
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
  bw   <- thmm.baumwelch(hmm0, obs$values)
  compare(hmm, bw$hmm)

  # adding constraints: mean bounds
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
  ctrl <- thmm.bw.ctrl(hmm0, lower.mean=c(-2,0), upper.mean=c(0,2), verbose=T)
  bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
  compare(hmm, bw$hmm)

  # adding constraint: as as previous but more general form
  hmm0 <- thmm.init(dnorm, 0.1, mean=c(0,0), sd=0.5)
  myconst <- function(params) {
    params$mean <- pmax(pmin(params$mean, c(0,2)), c(-2,0))
    params
  }
  ctrl <- thmm.bw.ctrl(hmm0, constraint=myconst, verbose=T)
  bw   <- thmm.baumwelch(hmm0, obs$values, ctrl)
  compare(hmm, bw$hmm)

}



