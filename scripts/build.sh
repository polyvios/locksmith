#!/usr/bin/env bash
cp build_funptr_table.py banshee/bin/build_funptr_table.py
eval $(opam config env)
./configure
make
