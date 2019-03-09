#
# some manual tests to compare to other packages : HiddenMarkov
#

if (FALSE) {

  set.seed(0)
  obs <- rnorm(200, 0, 1)

  require(thmm)

  h  <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=1)
  fc <- thmm.forward(h, obs)
  bc <- thmm.backward(h, obs)
  vc <- thmm.viterbi(h, obs, with.probseq=T)

  require(HiddenMarkov)

  fh <- forward(obs, h$trans, h$init, "norm", list(mean=c(-1,1), sd=1))
  dif <- abs(fh - fc$logalpha)
  max(dif)

  bh <- backward(obs, h$trans, "norm", list(mean=c(-1,1), sd=1))
  dif <- abs(bh - bc$logbeta)
  max(dif)

  vh <- Viterbi(dthmm(obs, h$trans, h$init, "norm", list(mean=c(-1,1), sd=1)))
  identical(vh, vc$states)

  # stability to overflow

  nobs <- obs
  nobs[100] <- 1000
  nfc <- thmm.forward(h, nobs, .useC=T)
  nfh <- forward(nobs, h$trans, h$init, "norm", list(mean=c(-1,1), sd=1))
  tail(nfc$logalpha, 1)
  tail(nfh, 1)
}

