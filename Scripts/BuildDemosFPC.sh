#!/bin/bash

echo "Building Demos using Free Pascal"
echo

source ./Common.sh

DEFINES="-dFULL_FEATURE_SET"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib
  -Fu$ROOTDIR/Extensions -Fu$ROOTDIR/Extensions/LibTiff -Fu$DEMOPATH/Common"
INCLUDE="-Fi$ROOTDIR/Source"
LIBS="-Fl$ROOTDIR/Extensions/J2KObjects"  

DEMOCOUNT=2

buildDemo "Benchmark/Bench.dpr" Bench
buildDemo "VampConvert/VampConvert.dpr" VampConvert

printResult