#!/bin/bash

echo "Building Demos using Kylix"

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal"
OUTPUT="-E$ROOTDIR/Demos/Bin"
UNITS="-U$ROOTDIR/Source -U$ROOTDIR/Source/JpegLib -U$ROOTDIR/Source/ZLib -U$DEMOPATH/Common
  -U$ROOTDIR/Extras/Extensions"
INCLUDE="-I$ROOTDIR/Source" 
OPTIONS=""

dcc $OPTIONS "$DEMOPATH/Benchmark/Bench.dpr" $OUTPUT $UNITS $INCLUDE
if test $? = 0; then
dcc $OPTIONS "$DEMOPATH/VampConvert/VampConvert.dpr" $OUTPUT $UNITS $INCLUDE
fi

if test $? = 0; then 
  echo "Demos successfuly build in Demos/Bin directory"
else
  echo "Error when building demos!"
fi

sh Clean.sh
