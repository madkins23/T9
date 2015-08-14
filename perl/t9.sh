#!/bin/sh
perl t9.pl --test < ../test/digits | diff -s ../test/results -
