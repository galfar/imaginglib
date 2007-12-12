#!/bin/bash

echo "Building libVampyreImaging.so using Free Pascal"

ROOTDIR=".."
LIBFILE="$ROOTDIR/Source/Projects/VampyreImaging.dpr"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib
  -Fu$ROOTDIR/Extras/Extensions"
INCLUDE="-Fi$ROOTDIR/Source" 
LIBS="-Fl$ROOTDIR/Extras/Extensions/J2KObjects"  
OUTPUT="-oVampyreImaging.so -FE$ROOTDIR/Bin"
OPTIONS="-Sgi -OG2 -Xs"

fpc $OPTIONS $OUTPUT $LIBFILE $UNITS $INCLUDE $LIBS

if test $? = 0; then 
  echo "Library successfuly build in Bin directory"
else
  echo "Error when building library!"
fi

sh Clean.sh
