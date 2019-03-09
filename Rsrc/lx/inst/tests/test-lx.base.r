# -------------------------------------------------
# $Id: test-lx.base.r 151 2016-11-12 19:38:05Z viari $
#
# lx.base test suite
#
# run as:
# require(testthat)
# test_dir("inst/tests/")
#

library(testthat)

context("LX base")

# -------------------------------------------------
test_that("package utilities", {

    x <- lx.system.file("test.dat")
    expect_that(basename(x), equals("test.dat"))
})

test_that("string utilities", {

    x <- lx.strtrim(" the lazy dog  ")
    expect_that(x, equals("the lazy dog"))
    x <- lx.strtrim(" the lazy dog  ", side="left")
    expect_that(x, equals("the lazy dog  "))
    x <- lx.strtrim(" the lazy dog  ", side="right")
    expect_that(x, equals(" the lazy dog"))
    
    x <- lx.strsplit("the lazy dog")
    expect_that(x, is_a("character"))
    expect_that(length(x), equals(3))
    
    
    x <- lx.regex.quote("the (lazy) dog")
    expect_that(x, equals("the \\(lazy\\) dog"))
})

# -------------------------------------------------
test_that("low level utilities", {

    foo <- function(...) {
        lx.args(-1)
    }
    x <- foo(first=1, second=anything, "third", FUN=function(x) x+1)
    expect_that(length(x), equals(4))
    expect_that(x[1], equals(list(first=1)))
    expect_that(x[2], equals(list(second=as.symbol("anything"))))
    expect_that(x[[3]], equals("third"))
    expect_that(x[[4]], is_a("call"))

    x <- lx.options(verbose)
    expect_that(is.character(x), is_true())
    x <- lx.options(atest=FALSE)
    expect_that(x, is_false())
    x <- lx.options(atest)
    expect_that(x, is_false())

})

# -------------------------------------------------
test_that("language utilities", {

    x <- lx.key.trans("red", c(red="rouge", blue="bleu"))
    expect_that(x, equals("rouge"))

    x <- lx.key.trans(c('red', 'pink'), c(red=1, green=2, blue=3, 0))
    expect_that(length(x), equals(2))
    expect_that(x[1], equals(1))
    expect_that(x[2], equals(0))

    x <- list(warm=c('red'), cold=c('blue', 'green'), basic=c('red', 'blue', 'green'))
    x <- lx.rev.dict(x)
    expect_that(x, equals(list(red=c("warm", "basic"),
                               blue=c("cold", "basic"),
                               green=c("cold", "basic"))))

    x <- lx.rotate(1:5, -2)
    expect_that(x, equals(c(4, 5, 1, 2, 3)))
    x <- lx.rotate(1:5, 2)
    expect_that(x, equals(c(3, 4, 5, 1, 2)))

    x <- lx.true()
    expect_that(x, is_true())
    x <- lx.false()
    expect_that(x, is_false())
    
    x <- lx.tobase(123, prefix="0x")
    expect_that(x, equals("0x7b"))
    x <- lx.tobase(123, base=2)
    expect_that(x, equals("1111011"))

})

# -------------------------------------------------
test_that("plot utilities", {

    x <- lx.mfrow(10)
    expect_that(x, equals(c(3, 4)))
    
    x <- lx.color.change('#ff0000', ds=-0.5)
    expect_that(x, equals("#FF8080"))
    x <- lx.color.change('red', ds=-0.5)
    expect_that(x, equals("#FF8080"))


    x <- lx.color.light('red', 50)
    expect_that(x, equals("#FF8080"))
    x <- lx.color.light('red', -50)
    expect_that(x, equals("#800000"))


    x <- lx.barplot(c(apple=10, orange=5, banana=15))
    x <- lx.barplot(c(apple=10, orange=5, banana=15),
                    col.text='white', col=lx.COLORS)
    
    plot(function(x) {sin(x*10)/x}, col="red", ylab="", main="main-plot")
    
    lx.plot.inset(function(x) {sin(x*10)/x},
                  ylab="", xlab="inset", cex.axis=0.5)
})

# -------------------------------------------------
test_that("contingency utilities", {

    data(lx.iris)
    
    x <- lx.table.margins(lx.table.bycols(lx.iris, by=1:2))
    expect_that(dim(x), equals(c(4, 3)))

    x <- lx.table.bycols(lx.iris)
    expect_that(dim(x), equals(c(3, 2, 2)))
    
    x <- lx.table.bycols(lx.iris, c('petal', 'sepal', 'species'))
    expect_that(dim(x), equals(c(2, 2, 3)))

    x <- lx.table.bypairs(lx.iris)    
    expect_that(length(x), equals(3))
    expect_that(dim(x[[1]]), equals(c(3, 2)))
    expect_that(dim(x[[2]]), equals(c(3, 2)))
    expect_that(dim(x[[3]]), equals(c(2, 2)))
    expect_that(names(x[1]), equals("species_by_petal"))
    expect_that(names(x[2]), equals("species_by_sepal"))
    expect_that(names(x[3]), equals("petal_by_sepal"))

    x <- lx.table.byfacts(lx.iris, by=2:3)
    expect_that(x, equals(matrix(c(100, 50, 67, 83), ncol=2, 
                            dimnames=list(c("large", "small"),
                                          c("petal", "sepal")))))


    x <- lx.table.bysets(list(a=c(1,2), b=c(1,3), c=c(2,3,4)))
    expect_that(as.integer(as.matrix(x)),
                equals(as.integer(matrix(c(2,1,1,1,2,1,1,1,3), ncol=3, 
                            dimnames=list(letters[1:3], letters[1:3])))))

    y <- lx.table.bysets(list(a=c(1,2,2), b=c(1,3), c=c(2,3,4,3)))
    expect_that(x, equals(y))

    x <- list(A=c('a', 'a', 'b'), B=c('a'), C=c('a', 'b', 'b', 'c'), D=c('c'))
    x <- lx.table.bymsets(x)
    y <- list(A=c(a=2, b=1), B=c(a=1), C=c(a=1, b=2, c=1), D=c(c=1))
    y <- lx.table.bymsets(y, as.card=TRUE)
    expect_that(dim(x), equals(c(4, 3)))
    expect_that(x, equals(y))

})


