#!/usr/bin/env bash
set -eo pipefail

# Builds and runs unit tests and Bench with third party shared libraries
# instead of the code compiled into Imaging: zlib, TurboJPEG (libjpeg-turbo
# 2.0+), libwebp and libtiff 4. The libraries must be installed (see
# CI.SetupLinux.sh) or, on Windows, be on PATH. The unit tests run with
# --dynlibs, which fails when a library is not found and Imaging falls back
# to its Pascal code (see Tests/UnitTests/TestImagingDynLibs.pas).
#
# Builds use -B and the usual unit output directories, so run this after the
# other build steps.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

# libwebp needs no define, it is used whenever it is found
DEFINES=(--opt="-dZLIB_USE_DYNLIB" --opt="-dJPEG_USE_TURBO_DYNLIB" --opt="-dTIFF_USE_LIBTIFF_DYNLIB")

cd "$REPO_ROOT/Tests/UnitTests"
lazbuild -B --bm="Debug" "${DEFINES[@]}" --opt="-o$REPO_ROOT/Bin/ImagingTestsDynLibs" "ImagingTests.lpi"

cd "$REPO_ROOT/Demos/ObjectPascal"
lazbuild -B --bm="Release" "${DEFINES[@]}" --opt="-o$REPO_ROOT/Demos/Bin/Bench-DynLibs" "Benchmark/Bench.lpi"

printf '\n===== Unit tests (shared libraries) =====\n'
cd "$REPO_ROOT/Bin"
./ImagingTestsDynLibs --dynlibs

printf '\n===== Bench (shared libraries) =====\n'
cd "$REPO_ROOT/Demos/Bin"
./Bench-DynLibs
