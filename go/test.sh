#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
TEST="$DIR/../test"

go run $DIR/numeric/numeric.go     < $TEST/digits | diff -s $TEST/results -
go run $DIR/odometer/odometer.go   < $TEST/digits | diff -s $TEST/results -
go run $DIR/recursive/recursive.go < $TEST/digits | diff -s $TEST/results -

