#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
TEST="$DIR/../test"
COMMON="--noinform --non-interactive --load infra.lisp --load"

sbcl $COMMON recursive.lisp --eval "(main #'recursive)" < $TEST/digits | diff -s $TEST/results -
# sbcl $COMMON odometer.lisp  --eval "(main #'odometer)"  < $TEST/digits | diff -s $TEST/results -
# sbcl $COMMON numeric.lisp   --eval "(main #'numeric)"   < $TEST/digits | diff -s $TEST/results -
