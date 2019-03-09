#
# $Id: Makefile 422 2019-01-07 22:01:07Z viari $
#

SHELL   = /bin/sh

.PHONY: all test clean

all: test

test:
	./test.basta.r
	./test.baf.r
	./test.tex.r

clean:
	-rm -f chr1_small.??? report.* tmp/* model.Rdata ploidy.Rdata
