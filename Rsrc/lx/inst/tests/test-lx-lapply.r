# -------------------------------------------------
# $Id: test-lx-lapply.r 120 2016-10-29 07:45:45Z viari $
#
# lx.apply test suite
#
# run as:
# library(testthat)
# test_dir("inst/tests/")
#

library(testthat)

context("LX apply")

# -------------------------------------------------
test_that("lapply", {

    
    n <- 10

    x <- lx.lapply(1:n, function(x) rnorm(5000*x), pg.verbose=FALSE, use.threads=FALSE)
    expect_that(length(x), equals(n))

    x <- lx.lapply(1:n, function(x) rnorm(5000*x), pg.verbose=FALSE, use.threads=TRUE)
    expect_that(length(x), equals(n))

    x <- lx.mapply(function(x, y) rnorm(5000*(x+y)), 1:n, 1:n, use.threads=FALSE)
    expect_that(length(x), equals(n))

    x <- lx.mapply(function(x, y) rnorm(5000*(x+y)), 1:n, 1:n, use.threads=TRUE)
    expect_that(length(x), equals(n))
    
})



