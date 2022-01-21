#!/bin/bash

# Ext. demos have some build and/or runtime dependencies like SDL or GL.
# libsdl1.2-dev package shoudl take care of both.
echo "Building Extended Demos using Free Pascal"
echo

source ./Common.sh

DEFINES="-dDONT_LINK_EXTRAS"
UNITS="-Fu$ROOTDIR/Source -Fu$ROOTDIR/Source/JpegLib -Fu$ROOTDIR/Source/ZLib -Fu$ROOTDIR/Extensions -Fu$DEMOPATH/Common"  
INCLUDE="-Fi$ROOTDIR/Source" 
LIBS=

DEMOCOUNT=2

buildDemo "SDLDemo/SDLDemo.dpr" SDLDemo
buildDemo "OpenGLDemo/OpenGLDemo.dpr" OpenGLDemo

printResult