#!/bin/bash

set -e
FPCTARGET=$(fpc -iTP)-$(fpc -iTO)

ROOTDIR=".."
DEMOPATH="$ROOTDIR/Demos/ObjectPascal" 
BINPATH="$ROOTDIR/Demos/Bin"
UNITPATH="$ROOTDIR/Demos/Bin/Dcu/$FPCTARGET"
# FPC does not like creating any new directories passed by -FE -FU
mkdir -p $UNITPATH
mkdir -p $BINPATH
set +e

OUTPUT="-FE$BINPATH -FU$UNITPATH"
# This is how you suppress -vn set in fpc.cfg
OPTIONS="-B -O3 -Xs -vn-"

DEMOSBUILD=0
DEMOCOUNT=0

function buildDemo {
  fpc $OPTIONS $OUTPUT $DEFINES $UNITS $INCLUDE $LIBS $DEMOPATH/$1 -o$2 
  if [ $? = 0 ]; then 
    ((DEMOSBUILD++))
  fi   
  echo
} 

function printResult {
  SWITCH="\033["
  NORMAL="${SWITCH}0m"
  RED="${SWITCH}0;31m"
  GREEN="${SWITCH}0;32m"

  if [ $DEMOSBUILD = $DEMOCOUNT ]; then
    echo -e "${GREEN}Build Successful - all $DEMOSBUILD of $DEMOCOUNT in Demos/Bin directory${NORMAL}"
  else
    echo -e "${RED}Errors during building - only $DEMOSBUILD of $DEMOCOUNT demos build${NORMAL}"
  fi
}
