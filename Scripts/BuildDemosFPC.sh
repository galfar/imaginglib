#!/bin/bash
set -eo pipefail

echo "Building Demos using Free Pascal"
echo

source ./DemosCommonFPC.sh

DEFINES="-dLINK_ALL_FILE_FORMATS"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/Libs
  -Fu$ROOTDIR/Extensions -Fu$ROOTDIR/Extensions/Libs -Fu$DEMOPATH/Common"

DEMOCOUNT=2

buildDemo "Benchmark/Bench.dpr"
buildDemo "VampConvert/VampConvert.dpr"

printResult