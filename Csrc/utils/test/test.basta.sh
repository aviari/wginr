#!/bin/sh
#
# $Id: test.basta.sh 396 2019-01-02 22:53:10Z viari $
#
# basta test driver
#

red='[0;31m' # red
grn='[0;32m' # green
mgn='[0;35m' # magenta

finish () {
  echo "$1$2[m: $bin"
  exit $st
}

bin=${bin:-`dirname -- $0`/$1}

/bin/echo -n "running:  $bin " > $bin.log 2>&1

if [ -f $bin.in ]; then
  ./$bin < $bin.in > $bin.raw 2>&1
else
  ./$bin > $bin.raw 2>&1
fi

st=$?

/bin/echo "status: $st" >> $bin.log 2>&1

if [ $st -ne 0 ]; then
  finish $red FAIL_RUN
fi

/bin/echo -n "checking: $bin " >> $bin.log 2>&1

egrep -v '^ *@' $bin.raw | sed -e 's/@.*//1' > $bin.out
diff --strip-trailing-cr $bin.out $bin.ref >> $bin.log 2>&1

st=$?

/bin/echo "status: $st" >> $bin.log 2>&1

if [ $st -ne 0 ]; then
  finish $mgn FAIL_DIF
fi

diff --brief $bin.bin $bin.bin.ref >> $bin.log 2>&1

st=$?

/bin/echo "status: $st" >> $bin.log 2>&1

if [ $st -ne 0 ]; then
  finish $mgn FAIL_DIF
fi

finish $grn OK