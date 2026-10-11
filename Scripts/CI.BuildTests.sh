#!/usr/bin/env bash
set -eo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

cd "$REPO_ROOT/Tests/UnitTests"

#  For custom named output executables with --opt="-o<name>", overriding output name from project file.
OUT_DIR="$REPO_ROOT/Bin"

lazbuild --bm="Debug" "ImagingTests.lpi"

if [ "$RUNNER_OS" == "Windows" ]; then
    # Tests of 32-bit build check also handling of sizes that do not fit into 32 bits
    lazbuild --cpu=i386 --os=win32 --opt="-o$OUT_DIR/ImagingTests32.exe" --bm="Debug" "ImagingTests.lpi"
fi
