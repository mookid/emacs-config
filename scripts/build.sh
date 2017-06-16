#!/bin/sh

if [ $# -ne 1 ]; then echo "usage: $0 <path-to-emacs-repository>"; exit 2; fi

cd $1
./autogen.sh
./configure --with-x-toolkit=lucid --without-pop CFLAGS='-O2'
make -j 4
sudo make install
