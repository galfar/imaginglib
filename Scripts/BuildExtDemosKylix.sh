#!/bin/bash

echo "Building Extension Demos using Kylix"

# Important! Set this dirs on your system 
SDLDIR=""
OPENGLDIR=""

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal"
OUTPUT="-E$ROOTDIR/Demos/Bin"
UNITS="-U$ROOTDIR/Source -U$ROOTDIR/Source/JpegLib -U$ROOTDIR/Source/ZLib -U$DEMOPATH/Common
  -U$ROOTDIR/Source/Extensions -U$ROOTDIR/Extras/Extensions -U$SDLDIR -U$OPENGLDIR"
INCLUDE="-I$ROOTDIR/Source -I$SDLDIR -I$OPENGLDIR" 
OPTIONS=""

dcc $OPTIONS "$DEMOPATH/SDLDemo/SDLDemo.dpr" $OUTPUT $UNITS $INCLUDE
if test $? = 0; then
dcc $OPTIONS "$DEMOPATH/OpenGLDemo/OpenGLDemo.dpr" $OUTPUT $UNITS $INCLUDE
fi

if test $? = 0; then 
  echo "Extension demos successfuly build in Demos/Bin directory"
else
  echo "Error when building demos!"
fi

sh Clean.sh
