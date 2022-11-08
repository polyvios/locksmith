#!/usr/bin/env bash

./locksmith --list-guardedby simple.c 2> out.txt
cat out.txt
grep -q "protected by" out.txt
