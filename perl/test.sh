#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
TEST="$DIR/../test"

perl $DIR/numeric.pl   < $TEST/digits | diff -s $TEST/results -
perl $DIR/odometer.pl  < $TEST/digits | diff -s $TEST/results -
perl $DIR/recursive.pl < $TEST/digits | diff -s $TEST/results -

