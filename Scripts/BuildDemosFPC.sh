#!/bin/bash

echo "Building Demos using Free Pascal"

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal" 
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib
  -Fu$ROOTDIR/Extras/Extensions -Fu$DEMOPATH/Common"
INCLUDE="-Fi$ROOTDIR/Source" 
OUTPUT="-FE$ROOTDIR/Demos/Bin"
OPTIONS="-Sgi -OG2 -Xs"

fpc $OPTIONS $OUTPUT "$DEMOPATH/Benchmark/Bench.dpr" $UNITS $INCLUDE -oBench
if test $? = 0; then
fpc $OPTIONS $OUTPUT "$DEMOPATH/VampConvert/VampConvert.dpr" $UNITS $INCLUDE -oVampConvert
fi

if test $? = 0; then 
  echo "Demos successfuly build in Demos/Bin directory"
else
  echo "Error when building demos!"
fi

sh Clean.sh
