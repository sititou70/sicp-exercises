#!/bin/sh
set -eux

# compile scheme to c
racket compile-to-c.rkt $1 >main.c

# compile c
CWD="$(pwd)"
cd ../ex5.51
make
cd "$CWD"

objs=$(find ../ex5.51/**/*.o -type f | grep -v "eval-apply" | xargs echo)
gcc -g -W -Wall -Wno-unused-parameter main.c -o main $objs
