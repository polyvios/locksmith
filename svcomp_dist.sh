#!/bin/sh
#run from root directory
ROOT=`pwd`
DIST="dist/locksmith"
mkdir -p $DIST
cat locksmith.in | sed -e "s.ROOT.\.." > $DIST/locksmith
chmod a+x $DIST/locksmith 
cp LICENSE simple.c $DIST
mkdir -p $DIST/cil/obj/x86_LINUX $DIST/cil/lib $DIST/cil/bin
cp cil/obj/x86_LINUX/cilly.* $DIST/cil/obj/x86_LINUX
cp cil/lib/*.pm $DIST/cil/lib
cp cil/bin/cilly $DIST/cil/bin
cat cil/bin/CilConfig.pm | sed -e "s.$ROOT.\.." > $DIST/cil/bin/CilConfig.pm

cd $DIST/..
zip -r locksmith.zip locksmith
