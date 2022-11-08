#!/usr/bin/env bash
eval $(opam config env)
opam install -y camlp4
./configure
make
