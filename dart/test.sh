#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"
TEST="$DIR/../test"

dart run bin/numeric.dart   < ../test/digits | diff -s "$TEST/results" -
dart run bin/odometer.dart  < ../test/digits | diff -s "$TEST/results" -
dart run bin/recursive.dart < ../test/digits | diff -s "$TEST/results" -
