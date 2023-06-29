#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
TEST="$DIR/../test"
SETUP="-t namespace.rkt -t infra.rkt"

racket $SETUP -t recursive.rkt -e '(main recursive)' < $TEST/digits | diff -s $TEST/results -
racket $SETUP -t odometer.rkt  -e '(main odometer)'  < $TEST/digits | diff -s $TEST/results -
racket $SETUP -t numeric.rkt   -e '(main numeric)'   < $TEST/digits | diff -s $TEST/results -
