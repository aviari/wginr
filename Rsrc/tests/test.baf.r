#!/usr/bin/env Rscript
#
# $Id: $
#
# functional test : baf format
#

require(xlx)

test.fail <- function(cond, ...) {
  lx.stopif(cond, paste('[0;31mFAIL: ', ..., '[m', sep=''))
}

fh <- baf.open("test.baf.bgz")

res <- baf.fetch.coord(fh, sloc2coord(fh, "carp:1:16611"))

idx <- which(rowSums(res) != 0)
test.fail(! all(c(211, 1261, 4831, 4832, 4833) %in% idx), "test baf failure carp1")

res <- baf.fetch.coord(fh, sloc2coord(fh, "balist:1:16554"))

idx <- which(rowSums(res) != 0)
test.fail(length(idx) != 0,  "test baf failure balist1")

res <- baf.heterozygous(fh, deltafreq=NA)
test.fail(! all(rownames(res) == "3.571"),  "test baf failure baf.heterozygous")

fh <- baf.close(fh)

cat('[0;32mOK[m: test.baf.r\n')

quit()

