#!/usr/bin/env bash
set -eo pipefail

# Runs WASM/WASI unit tests and demos cross-built by CI.BuildCross.sh with wasmtime.
#
# Usage: CI.RunWasm.sh [suffix]   (default suffix wasm32 -> ImagingTests-wasm32.wasm etc.)
#
# WASI programs can only access preopened directories. Notes on how FPC WASI RTL
# (trunk 2026-11) and wasmtime behave:
# - wasmtime passes just the file name as argv[0] and FPC RTL uses the first
#   preopened directory as the current directory. So the executable's directory
#   is preopened first: ParamStr(0) then expands to the real executable path,
#   and Demos/Data is found by walking up from it as on other platforms
#   (repository root is preopened too for that, at the same path as on host).
# - ForceDirectories cannot create a directory directly in a preopened
#   directory, so the test output directory is created here.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

SUFFIX="${1:-wasm32}"

# Runs WASM executable given as <dir> <file> [args...]
function runWasm {
  local DIR="$1" FILE="$2"
  shift 2
  cd "$DIR"
  wasmtime run --dir="$DIR" --dir="$REPO_ROOT" "$FILE" "$@"
}

mkdir -p "$REPO_ROOT/Bin/TestOut"

# Test program returns non-zero exit code when some test fails
printf '\n===== Unit tests (%s) =====\n' "$SUFFIX"
runWasm "$REPO_ROOT/Bin" "ImagingTests-$SUFFIX.wasm"

printf '\n===== Bench (%s) =====\n' "$SUFFIX"
runWasm "$REPO_ROOT/Demos/Bin" "Bench-$SUFFIX.wasm"

printf '\n===== VampConvert (%s) =====\n' "$SUFFIX"
runWasm "$REPO_ROOT/Demos/Bin" "VampConvert-$SUFFIX.wasm" \
  -i=../Data/Tigers.jpg -o=./conv-Tigers-$SUFFIX.png \
  -format=A8R8G8B8 -resize=1600x1400xlanczos -rotate=45
