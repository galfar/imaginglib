#!/bin/bash

echo "Building libVampyreImaging.so using Kylix"

ROOTDIR=".."
LIBFILE="$ROOTDIR/Source/Projects/VampyreImaging.dpr"
OUTPUT="-E$ROOTDIR/Bin"
UNITS="-U$ROOTDIR/Source -U$ROOTDIR/Source/JpegLib -U$ROOTDIR/Source/ZLib"
INCLUDE="-I$ROOTDIR/Source" 
OPTIONS=""

dcc $OPTIONS $LIBFILE $OUTPUT $UNITS $INCLUDE

if test $? = 0; then 
  echo "Library successfuly build in Bin directory"
else
  echo "Error when building library!"
fi

sh Clean.sh 
