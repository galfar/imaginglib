#!/bin/bash

echo "Building Extended Demos using Free Pascal"
echo

set -e
FPCTARGET=$(fpc -iTP)-$(fpc -iTO)

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal" 
UNITPATH="$ROOTDIR/Demos/Bin/Dcu/$FPCTARGET"
BINPATH="$ROOTDIR/Demos/Bin"
# FPC does not like creating any new directories passed by -FE -FU
mkdir -p $UNITPATH
mkdir -p $BINPATH
# There could be units compiled with other defines 
find $UNITPATH -mindepth 1 -delete
set +e

DEFINES="-dDONT_LINK_EXTRAS"
OUTPUT="-FE$BINPATH -FU$UNITPATH"
# This is how you suppress -vn set in fpc.cfg
OPTIONS="-O3 -Xs -vn-"

UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib \
  -Fu$DEMOPATH/Common -Fu$ROOTDIR/Extensions"  
INCLUDE="-Fi$ROOTDIR/Source" 
LIBS=

DEMOS_BUILD=0
DEMO_COUNT=2

function buildDemo {
  fpc $OPTIONS $OUTPUT $DEFINES $UNITS $INCLUDE $LIBS $1 -o$2 
  if [ $? = 0 ]; then 
    ((DEMOS_BUILD++))
  fi   
  echo
} 

buildDemo "$DEMOPATH/SDLDemo/SDLDemo.dpr" SDLDemo
buildDemo "$DEMOPATH/OpenGLDemo/OpenGLDemo.dpr" OpenGLDemo

if [ $DEMOS_BUILD = $DEMO_COUNT ]; then
  echo "Demos successfuly build in Demos/Bin directory"
else
  echo "Error when building demos!"
fi


