# -------------------------------------------------
# $Id: test-xlx.swiss.r 120 2016-10-29 07:45:45Z viari $
#
# mdb.swiss test suite
#
# run as:
# library(testthat)
# test_dir("inst/tests/")
#

library(testthat)

context("MDB Swiss")

# -------------------------------------------------
test_that("mdb swiss", {


    sink(file("tests.log", open="at"), type="message")

    test.file <- lx.system.file('samples/test_swiss.dat')
    
    # workaround for bug in rstudio test package
    # that does not setup path.package correcly
    
    if (! file.exists(test.file))
      test.file <- lx.system.file('inst/samples/test_swiss.dat')

    if (file.exists(test.file)) {
      db <- mdb.swiss.read(test.file)
      expect_that(length(db), equals(5))
      expect_that(names(db), equals(c("P05100", "P04395", "P00350", "P52697", "P46482")))
      expect_that(db$P04395$OC[1], equals("Bacteria"))

      x <- mdb.find(db, 'KW', 'gluconate', ignore.case=TRUE)
      expect_that(x, equals("P00350"))

      x <- mdb.find(db, 'KW', 'glu', ignore.case=TRUE)
      expect_that(x, equals(c("P00350", "P52697")))
    }
    else {
      cat('samples/test_swiss.dat: not found\n') 
    }
    
    sink(type="message")
    unlink("tests.log")
})

