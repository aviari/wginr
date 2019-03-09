# -------------------------------------------------
# $Id: test_thmm.r 425 2019-01-07 22:14:07Z viari $
#
# thmm test suite
#
# run as:
# require(testthat)
# test_dir("inst/tests/")
#

require(testthat)
require(thmm)

context("thmm base")

test_that("R and C calls", {

  h <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
  obs <- rnorm(200, 0, 1)

  fr <- thmm.forward(h, obs, .useC=F)
  fc <- thmm.forward(h, obs, .useC=T)
  expect_equal(fr$logalpha, fc$logalpha)

  br <- thmm.backward(h, obs, .useC=F)
  bc <- thmm.backward(h, obs, .useC=T)
  expect_equal(br$logbeta, bc$logbeta)

  fbr <- thmm.forward.backward(h, obs, .useC=F)
  fbc <- thmm.forward.backward(h, obs, .useC=T)
  expect_equal(fbr$logalpha, fbc$logalpha)
  expect_equal(fbr$logbeta, fbc$logbeta)
  expect_equal(fbr$gamma, fbc$gamma)
  expect_equal(fbr$logrho, fbc$logrho)

  vr <- thmm.viterbi(h, obs, with.probseq=T, .useC=F)
  vc <- thmm.viterbi(h, obs, with.probseq=T, .useC=T)
  expect_equal(vr$states, vc$states)
  expect_equal(vr$nu, vc$nu)
  expect_equal(vr$logprobseq, vc$logprobseq)
})

test_that("density def", {

  obs <- rnorm(200, 0, 1)

  h <- thmm.init(dnorm, 0.1, mean=c(-1,1))
  llk <- thmm.forward(h, obs)$loglike

  h <- thmm.init("dnorm", 0.1, mean=c(-1,1))
  expect_equal(thmm.forward(h, obs)$loglike, llk)

  foo <- dnorm
  h <- thmm.init(foo, 0.1, mean=c(-1,1))
  expect_equal(thmm.forward(h, obs)$loglike, llk)

  foo <- "dnorm"
  h <- thmm.init(foo, 0.1, mean=c(-1,1))
  expect_equal(thmm.forward(h, obs)$loglike, llk)

  foo <- function(...) dnorm(...)
  h <- thmm.init(foo, 0.1, mean=c(-1,1))
  expect_equal(thmm.forward(h, obs)$loglike, llk)

  foo <- function() "dnorm"
  h <- thmm.init(foo(), 0.1, mean=c(-1,1))
  expect_equal(thmm.forward(h, obs)$loglike, llk)

})

test_that("density params", {

  expect_is(thmm.init(dnorm, 0.1, mean=c(-1,1)), "DTHmm")
  m <- c(-1,1)
  expect_is(thmm.init(dnorm, 0.1, mean=m), "DTHmm")
  foo <- function() c(-1, 1)
  expect_is(thmm.init(dnorm, 0.1, mean=foo()), "DTHmm")

})

test_that("environments", {

  h <- thmm.init(dnorm, 0.1, mean=c(-1,1))
  obs <- rnorm(200, 0, 1)
  llk <- thmm.forward(h, obs)$loglike

  foo <- function() {hmm<-thmm.init(dnorm, 0.1, mean=c(-1,1)); thmm.forward(hmm, obs)}
  expect_equal(foo()$loglike, llk)

  foo <- function() {hmm<-thmm.init("dnorm", 0.1, mean=c(-1,1)); thmm.forward(hmm, obs)}
  expect_equal(foo()$loglike, llk)

  foo <- function() {bar<-dnorm; hmm<-thmm.init(bar, 0.1, mean=c(-1,1)); thmm.forward(hmm, obs)}
  expect_equal(foo()$loglike, llk)

  foo <- function() {bar<-dnorm; hmm<-thmm.init("bar", 0.1, mean=c(-1,1)); thmm.forward(hmm, obs)}
  expect_equal(foo()$loglike, llk)

  foo <- function() {bar<-dnorm; foo<-"bar"; hmm<-thmm.init(foo, 0.1, mean=c(-1,1)); thmm.forward(hmm, obs)}
  expect_equal(foo()$loglike, llk)
})

test_that("gaussian mixture", {

  # gaussian mixture
  #
  dmixnorm <- function(x, mean=0, sd=1, prob=1, log=FALSE) {
    msp <- mapply(function(m, s, p) list(mean=m, sd=s, prob=p), mean, sd, prob, SIMPLIFY=F)
    unlist(lapply(x, function(x) sapply(msp, function(p)
      sum(p$prob*dnorm(x, mean=p$mean, sd=p$sd)))))
  }

  rmixnorm <- function(n, mean=0, sd=1, prob=1, log=FALSE) {
    .cycle <- function(x, n) head(rep(x, n), n)
    msp <- mapply(function(m, s, p) list(mean=m, sd=s, prob=p), mean, sd, prob, SIMPLIFY=F)
    unlist(lapply(seq_len(n), function(i) sapply(msp, function(p) {
      r <- mapply(function(m, s) rnorm(1, m, s), p$mean, p$sd)
      sample(r, size=1, prob=.cycle(p$prob, length(r)))
    })))
  }

  hmm <- thmm.init(dmixnorm, 0.1, mean=list(c(-1,1), 0), sd=list(0.1, 0.1),
                   prob=list(c(0.1,0.9), 1))
  set.seed(0)
  x <- thmm.simulate(hmm, n=1000, with.values=TRUE)
  v <- thmm.viterbi(hmm, x$values, .useC=T)
  y <- abs(sapply(thmm.parameters(hmm, "mean")[v$states], function(x) x[1]))
  my <- median(abs(y-abs(x$values)))
  expect_lte(my, 0.1)

})

test_that("sample", {
  trans <- matrix(c(.1,.5,.9,.5), 2, 2)
  hmm <- thmm.init(dnorm, trans, c(.5,.5), mean=c(1,-1), sd=1)
  obs <- c(-1.6835, 0.0635, -2.1688, 0.3043, -0.3188, -0.7835, 1.0398, -1.3558, 1.0882, 0.4050)
  vit <- thmm.viterbi(hmm, obs, with.probseq=T)
  expect_equal(vit$loglike, -15.226179571)
  expect_identical(vit$states, c(2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L))
})
