#!/usr/bin/env bash
set -eo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

cd "$REPO_ROOT/Demos/ObjectPascal"

#  For custom named output executables with --opt="-o<name>", overriding output name from project file.
OUT_DIR="$REPO_ROOT/Demos/Bin"

# Built for all platforms
lazbuild --bm="Release" "Benchmark/Bench.lpi"
lazbuild --bm="Debug" --opt="-o$OUT_DIR/Bench-Debug" "Benchmark/Bench.lpi"
lazbuild --bm="Release" "VampConvert/VampConvert.lpi"

OPTS=""
if [[ "$RUNNER_OS" == "macOS" ]]; then
    # Need to specify some older macOS as minimum version, otherwise the LCL linking may fail (new XCode, latest ARM macOS) with "Error: ld: malformed method list atom 'ltmp5' "
    OPTS=--opt="-WM10.14"
fi

lazbuild --bm="Release" $OPTS "LCLImager/lclimager.lpi"
lazbuild --bm="Release" $OPTS "ImageBrowser/ImgBrowser.lpi"

if [ "$RUNNER_OS" == "Linux" ]; then
    # For Linux build LCL demos also with Qt
    # Needs to have "libqt5pas-dev" + "libqt6pas-dev" package installed
    lazbuild --ws=qt5 --bm="Release" --opt="-o$OUT_DIR/LCLImage-Qt5" "LCLImager/lclimager.lpi"
    lazbuild --ws=qt5 --bm="Release" --opt="-o$OUT_DIR/ImgBrowser-Qt5" "ImageBrowser/ImgBrowser.lpi"
    lazbuild --ws=qt6 --bm="Release" --opt="-o$OUT_DIR/LCLImage-Qt6" "LCLImager/lclimager.lpi"
    # Test also GTK3, soon to be default for LCL on Linux.
    # TODO: When this happens, change this test build to GTK2.
    lazbuild --ws=gtk3 --bm="Release" --opt="-o$OUT_DIR/LCLImage-GTK3" "LCLImager/lclimager.lpi"
fi

if [[ "$RUNNER_OS" != "macOS" && "$RUNNER_ARCH" != "ARM64" ]]; then
    # Build these for non macOS platforms
    lazbuild --bm="Release" "OpenGLDemo/OpenGLDemo.lpi"
    lazbuild --bm="Release" "SDLDemo/SDLDemo.lpi"
fi

if [ "$RUNNER_OS" == "Windows" ]; then
    # Build D3D demo just for Windows
    lazbuild --bm="Release" "D3DDemo/D3DDemo.lpi"
    # Build 32-bit version of Bench to check that cross-compilation works
    lazbuild --cpu=i386 --os=win32 --opt="-o$OUT_DIR/Bench32.exe" --bm="Release" "Benchmark/Bench.lpi"
fi
