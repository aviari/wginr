#!/bin/sh
#
# $Id: test.bam2baf.sh 396 2019-01-02 22:53:10Z viari $
#
# fasta2basta test driver
#

export PATH=..:../../ports/`uname -s`/bin:$PATH

red='[0;31m' # red
grn='[0;32m' # green
mgn='[0;35m' # magenta

finish () {
  echo "$1$2[m: $bin"
  exit $st
}

bin=bam2baf

/bin/echo -n "running:  $bin " > test.$bin.log 2>&1

$bin test.$bin.in test.$bin.out > test.$bin.log 2>&1

st=$?

/bin/echo "status: $st" >> test.$bin.log 2>&1

if [ $st -ne 0 ]; then
  finish $red FAIL_RUN
fi

/bin/echo -n "checking: $bin " >> test.$bin.log 2>&1

diff --brief test.$bin.out test.$bin.ref >> test.$bin.log 2>&1

st=$?

/bin/echo "status: $st" >> test.$bin.log 2>&1

if [ $st -ne 0 ]; then
  finish $mgn FAIL_DIF
fi

finish $grn OK
