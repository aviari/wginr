#!/usr/bin/env Rscript
#
# $Id: test.tex.r 120 2016-10-29 07:45:45Z viari $
#
# internal test
#

require(lx)

test.fail <- function(cond, ...) {
  lx.stopif(cond, paste('[0;31mFAIL: ', ..., '[m', sep=''))
}

tex <- tex.open("report")

tex <- tex.section(tex, "Section")
tex <- tex.fig.on(tex)
plot(1:10, col=1:10, pch=19, cex=3)
tex <- tex.fig.off(tex)

tex <- tex.close(tex)

cat('[0;32mOK[m: test.tex.r\n')

quit()
