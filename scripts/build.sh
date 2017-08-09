#!/bin/sh

set -e

if [ $# -ne 1 ]; then echo "usage: $0 <path-to-emacs-repository>"; exit 2; fi

cd $1

echo running autogen.sh...
./autogen.sh > autogen.out 2> autogen.err

echo running configure script...
./configure \
    --without-makeinfo\
    --with-x-toolkit=lucid\
    --without-pop\
    CFLAGS='-O2'\
    > configure.out 2> configure.err

echo running make...
make -j 4 > build.out 2> build.err

echo ok
