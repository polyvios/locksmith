#!/usr/bin/env bash

eval $(opam config env)
./configure
if ! make; then
  echo "Rebuild banshee"
  cd banshee
  make
  git checkout -- .
  git clean -df
  make
  echo "Now retry Locksmith"
  cd ..
  make
fi
