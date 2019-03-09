#
# $Id: Makefile 396 2019-01-02 22:53:10Z viari $
#

_SCRIPTDIR := ../../../scripts

include ../Makefile.common

# -------------------------------------
# test sources
#

TEST_SRC = $(wildcard test.*.c)

TEST_PRG = $(TEST_SRC:.c=)  \
           test.fasta2basta \
           test.bam2baf

CFLAGS = -g -O2 $(OSFLAGS)

LDFLAGS = -L..

LDLIBS = -llx

# -------------------------------------
# rules
#

.PHONY: all test clean

all: test

test: $(TEST_PRG)
	./test-all $(TEST_PRG)

clean:
	-$(RM) -f -r $(TEST_PRG) *.dSYM *.log *.raw *.out *.bak
