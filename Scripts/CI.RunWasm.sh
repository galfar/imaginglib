#!/usr/bin/env bash
set -eo pipefail

# Runs WASM/WASI unit tests and demos cross-built by CI.BuildCross.sh with wasmtime.
#
# Usage: CI.RunWasm.sh [suffix]   (default suffix wasm32 -> ImagingTests-wasm32.wasm etc.)
#
# WASI programs can only access preopened directories. Repository root is
# preopened at the same path as on host and executables are passed with absolute
# paths, so ParamStr(0) is absolute and the data directory (Demos/Data) is found
# by walking up from the executable directory as on other platforms.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

SUFFIX="${1:-wasm32}"
RUN=(wasmtime run --dir="$REPO_ROOT")

cd "$REPO_ROOT/Bin"

# Test program returns non-zero exit code when some test fails
printf '\n===== Unit tests (%s) =====\n' "$SUFFIX"
"${RUN[@]}" "$REPO_ROOT/Bin/ImagingTests-$SUFFIX.wasm"

cd "$REPO_ROOT/Demos/Bin"

printf '\n===== Bench (%s) =====\n' "$SUFFIX"
"${RUN[@]}" "$REPO_ROOT/Demos/Bin/Bench-$SUFFIX.wasm"

printf '\n===== VampConvert (%s) =====\n' "$SUFFIX"
"${RUN[@]}" "$REPO_ROOT/Demos/Bin/VampConvert-$SUFFIX.wasm" \
  -i="$REPO_ROOT/Demos/Data/Tigers.jpg" -o="$REPO_ROOT/Demos/Bin/conv-Tigers-$SUFFIX.png" \
  -format=A8R8G8B8 -resize=1600x1400xlanczos -rotate=45
