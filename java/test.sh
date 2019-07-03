#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
SRC="$DIR/src/org/doorways/t9"
TEST="$DIR/../test"

java $SRC/Numeric.java   < $TEST/digits | diff -s $TEST/results -
java $SRC/Odometer.java  < $TEST/digits | diff -s $TEST/results -
java $SRC/Recursive.java < $TEST/digits | diff -s $TEST/results -

