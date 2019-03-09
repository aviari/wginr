#!/usr/bin/env Rscript
#
# $Id: test.basta.r 422 2019-01-07 22:01:07Z viari $
#
# functional test : basta format
#

require(xlx)

args <- lx.getargs()

test.fail <- function(cond, ...) {
  lx.stopif(cond, paste('[0;31mFAIL: ', ..., '[m', sep=''))
}
  
fh <- basta.open("test.bst.gz")
s <- basta.fetch.sloc(fh, "seq2:1-4")
test.fail(s != "GCGC", "badaboum")
fh <- basta.close(fh)

dobig <- (! is.null(args$dobig)) && args$dobig

bigfile <- "../../data/HS_GRCh37_72.bst.gz"

if (dobig && file.exists(bigfile)) {
 fh <- basta.open(bigfile)
 s <- basta.fetch.sloc(fh, "1:1000001-1000010")
 test.fail(s != "GGGCACAGCC", "1: bad sequence")
 s <- basta.fetch.sloc(fh, "3:1000001-1000010")
 test.fail(s != "CCAACAAGCA", "1: bad sequence")
 s <- basta.fetch.sloc(fh, "20:1000001-1000010")
 test.fail(s != "TGGGAGAGAA", "1: bad sequence")

 seq <- fh$header$seq
 regions <- basta2clocs(fh)
 samp <- coords.sample(fh, regions, size=1000)
 clocs <- coords2clocs(fh, samp)

 fh <- basta.close(fh)
}

bed <- bed.read("test.bed")
test.fail(length(bed) != 3, "bed read length error")
test.fail(bed[2,'to'] != 24, "bed read value error")

cat('[0;32mOK[m: test.basta.r\n')

quit()
