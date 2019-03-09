#
# some manual tests to compare to other packages : RHmm
#

if (FALSE) {

  set.seed(0)
  obs <- rnorm(200, 0, 1)

  require(thmm)

  h   <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=1)
  fbc <- thmm.forward.backward(h, obs)
  vc  <- thmm.viterbi(h, obs, with.probseq=T)

  require(RHmm)
  rhmm <- HMMSet(h$init, h$trans, "NORMAL", mean=c(-1,1), var=c(1,1))

  fbx <- forwardBackward(rhmm, obs, logData=TRUE)
  dif <- abs(fbx$Alpha - fbc$logalpha)
  max(dif)

  dif <- abs(fbx$Beta - fbc$logbeta)
  max(dif)

  dif <- abs(fbx$Gamma - fbc$gamma)
  max(dif)

  dif <- abs(fbx$Rho - fbc$logrho)
  max(dif)

  abs(fbx$LLH - fbc$loglike)

  vx <- viterbi(rhmm, obs)
  identical(as.integer(vx$states), vc$states)
  abs(vx$logViterbiScore - vc$logviterbi)
  abs(vx$logProbSeq - vc$logprobseq)

  # stability to overflow

  nobs <- obs
  nobs[100] <- 1000
  nfc <- thmm.forward(h, nobs, .useC=T)
  nfbx <- forwardBackward(rhmm, nobs, logData=TRUE)
  tail(nfc$logalpha, 1)
  tail(nfbx$Alpha, 1)
}
