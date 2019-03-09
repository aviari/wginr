# -------------------------------------------------
# $Id: test-lx.globals.r 151 2016-11-12 19:38:05Z viari $
#
# lx.globals test suite
#
# run as:
# library(testthat)
# test_dir("inst/tests/")
#

library(testthat)

context("LX options")

# -------------------------------------------------
test_that("options", {

    x <- lx.options(verbose)
    expect_that(is.character(x), is_true())

    x <- lx.options(atest=1)
    expect_that(x, equals(c(atest=1)))
    expect_that(x==1, is_true())

    x <- lx.options(atest)
    expect_that(x, equals(c(atest=1)))
    expect_that(x, is_equivalent_to(1))

    x <- lx.options()
    expect_that(x, is_a("list"))
    expect_that(length(x) > 0, is_true())
    
    x <- lx.COLORS
    expect_that(length(x), equals(10))
    
})



