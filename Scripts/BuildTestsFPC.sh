#!/bin/bash
set -eo pipefail

echo "Building Unit Tests using Free Pascal"
echo

source ./DemosCommonFPC.sh

TESTPATH="$ROOTDIR/Tests/UnitTests"
BINPATH="$ROOTDIR/Bin"
UNITPATH="$ROOTDIR/Bin/Dcu/$FPCTARGET/ImagingTests"
mkdir -p $UNITPATH

OUTPUT="-FE$BINPATH -FU$UNITPATH"
# Tests are built with debug info, assertions, heap trace (memory leaks are listed at exit),
# and IO, range, overflow, and object checks. This is how you suppress -vn set in fpc.cfg
OPTIONS="-B -O1 -gl -gh -Sa -CroiR -vn-"
DEFINES="-dLINK_ALL_FILE_FORMATS"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/Libs
  -Fu$ROOTDIR/Extensions -Fu$ROOTDIR/Extensions/Libs -Fu$ROOTDIR/Extensions/LibTiff"

$FPC $OPTIONS $OUTPUT $TARGET $DEFINES $UNITS $INCLUDE $LIBS $TESTPATH/ImagingTests.dpr

echo
echo "Build Successful - run $BINPATH/ImagingTests"
