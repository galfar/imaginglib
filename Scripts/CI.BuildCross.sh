#!/usr/bin/env bash
set -eo pipefail

# Cross-builds unit tests and console demos with lazbuild for the given target.
# Cross compiler must be installed (see CI.InstallFPCCross.sh).
#
# Usage: CI.BuildCross.sh <cpu> <os> <suffix> [ext]
#   Output executables are named <name>-<suffix><ext>,
#   e.g. CI.BuildCross.sh riscv64 linux riscv64       -> Bin/ImagingTests-riscv64
#        CI.BuildCross.sh wasm32 wasip1 wasm32 .wasm  -> Bin/ImagingTests-wasm32.wasm

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

CPU="$1"
OS="$2"
SUFFIX="$3"
EXT="$4"
if [[ -z "$CPU" || -z "$OS" || -z "$SUFFIX" ]]; then
  echo "Usage: $0 <cpu> <os> <suffix> [ext]" >&2
  exit 2
fi

TARGET=(--cpu="$CPU" --os="$OS")

TEST_OPTS=()
DEMO_OPTS=()
if [[ "$CPU" == "wasm32" ]]; then
  # Workarounds for FPC trunk (2026-11) WASM issues:
  # - Debug mode of tests uses external debug info (-Xg), which produces a WASM
  #   module that wasmtime fails to parse ("section size mismatch")
  # - FPC crashes with access violation when linking demos with smart linking
  #   (Release mode -CX -XX)
  TEST_OPTS=(--opt="-Xg-")
  DEMO_OPTS=(--opt="-CX-" --opt="-XX-")
fi

printf '\n===== Unit tests (%s-%s) =====\n' "$CPU" "$OS"
cd "$REPO_ROOT/Tests/UnitTests"
lazbuild "${TARGET[@]}" --bm="Debug" "${TEST_OPTS[@]}" --opt="-o$REPO_ROOT/Bin/ImagingTests-$SUFFIX$EXT" "ImagingTests.lpi"

printf '\n===== Demos (%s-%s) =====\n' "$CPU" "$OS"
cd "$REPO_ROOT/Demos/ObjectPascal"
OUT_DIR="$REPO_ROOT/Demos/Bin"
lazbuild "${TARGET[@]}" --bm="Release" "${DEMO_OPTS[@]}" --opt="-o$OUT_DIR/Bench-$SUFFIX$EXT" "Benchmark/Bench.lpi"
lazbuild "${TARGET[@]}" --bm="Release" "${DEMO_OPTS[@]}" --opt="-o$OUT_DIR/VampConvert-$SUFFIX$EXT" "VampConvert/VampConvert.lpi"

ls -lh "$REPO_ROOT/Bin/ImagingTests-$SUFFIX$EXT" "$OUT_DIR/Bench-$SUFFIX$EXT" "$OUT_DIR/VampConvert-$SUFFIX$EXT"
