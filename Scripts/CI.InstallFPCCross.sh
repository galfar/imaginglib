#!/usr/bin/env bash
set -eo pipefail

# Adds FPC cross compiler for the given target to FPC trunk installed
# by fpclazup (ollydev/setup-lazarus action with fpclazup-* inputs).
#
# Usage: CI.InstallFPCCross.sh <cpu> <os>
#   e.g. CI.InstallFPCCross.sh powerpc64 linux  (little endian ELFv2 on Linux)
#        CI.InstallFPCCross.sh riscv64 linux
#        CI.InstallFPCCross.sh wasm32 wasip1
#
# Needs FPCLAZUP_URL env variable with the same URL as used by setup-lazarus.
# Cross binutils for Linux targets are downloaded by fpclazup (--autotools),
# target libc comes from Ubuntu libc6-dev-*-cross packages (see crossLibDir
# in CI.Common.sh), which must be installed.
# For wasm32-wasip1 nothing is downloaded by fpclazup: those downloads failed
# often and fpclazup then crashed. wasm-ld (and llvm-mc) come from lld/llvm
# packages of Ubuntu and libc.a, which fpclazup requires for the target, from
# wasi-libc; they are installed here (sudo apt-get) in every run, because the
# cached installation refers to them.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

CPU="$1"
OS="$2"
if [[ -z "$CPU" || -z "$OS" ]]; then
  echo "Usage: $0 <cpu> <os>" >&2
  exit 2
fi

if [[ -z "$RUNNER_TEMP" ]]; then
  echo "ERROR: RUNNER_TEMP not set" >&2
  exit 2
fi

# Locations used by setup-lazarus action
INSTALL_DIR="$RUNNER_TEMP/fpclazup/"
FPCLAZUP_BIN="$RUNNER_TEMP/fpclazupbin"

case "$CPU" in
  powerpc64) PPCROSS=ppcrossppc64 ;;
  riscv64)   PPCROSS=ppcrossrv64 ;;
  wasm32)    PPCROSS=ppcrosswasm32 ;;
  *)         PPCROSS=ppcross ;;
esac

CROSS_OPT="$(crossOptions "$CPU" "$OS")"
CROSS_LIB_DIR="$(crossLibDir "$CPU" "$OS")"
CROSS_BIN_DIR=""
if [[ -n "$CROSS_LIB_DIR" ]] && [[ ! -e "$CROSS_LIB_DIR/libc.so" ]]; then
  echo "ERROR: Target libc not found in $CROSS_LIB_DIR (install libc6-dev-*-cross package)" >&2
  exit 2
fi

if [[ "$CPU" == "wasm32" && "$OS" == "wasip1" ]]; then
  sudo apt-get install -y -q lld llvm wasi-libc
  CROSS_LIB_DIR=$(dirname "$(dpkg -L wasi-libc | grep -E '/wasm32-wasi(p1)?/libc\.a$' | head -1)")
  if [[ ! -f "$CROSS_LIB_DIR/libc.a" ]]; then
    echo "ERROR: libc.a of wasi-libc not found" >&2
    exit 2
  fi
  echo "wasi-libc: $CROSS_LIB_DIR"

  CROSS_BIN_DIR="$INSTALL_DIR/cross-system/$CPU-$OS/bin"
  mkdir -p "$CROSS_BIN_DIR"
  for TOOL in wasm-ld llvm-mc; do
    TOOL_PATH=$(command -v "$TOOL" || ls /usr/lib/llvm-*/bin/"$TOOL" 2>/dev/null | sort -V | tail -1)
    if [[ -z "$TOOL_PATH" ]]; then
      echo "ERROR: $TOOL not found" >&2
      exit 2
    fi
    # Also with the binutils prefix of the target, in case it is used
    ln -sf "$TOOL_PATH" "$CROSS_BIN_DIR/$TOOL"
    ln -sf "$TOOL_PATH" "$CROSS_BIN_DIR/$CPU-$OS-$TOOL"
  done
  "$CROSS_BIN_DIR/wasm-ld" --version
fi

# Records options the installed cross compiler and its RTL were built with
MARKER="$INSTALL_DIR/ci-cross-$CPU-$OS.txt"
MARKER_TEXT="crossopt=$CROSS_OPT crosslibdir=$CROSS_LIB_DIR crossbindir=$CROSS_BIN_DIR"

# Cached FPC installation may already have the cross compiler. setup-lazarus
# saves the installation in its post step only when its cache key was missing,
# apparently also when the job fails, so a cache without the cross compiler is
# possible: then it is built in every run (until that cache expires or is
# deleted). Reused only when it was built by this script with the same options.
if [[ "$PPCROSS" != "ppcross" ]] && [[ -n "$(find "$INSTALL_DIR" -type f -name "$PPCROSS" -print -quit)" ]] &&
   [[ -f "$MARKER" ]] && [[ "$(cat "$MARKER")" == "$MARKER_TEXT" ]]; then
  echo "Cross compiler $PPCROSS for $CPU-$OS already installed ($MARKER_TEXT)"
else
  # setup-lazarus downloads fpclazup only when FPC is not restored from cache
  if [[ ! -x "$FPCLAZUP_BIN" ]]; then
    if [[ -z "$FPCLAZUP_URL" ]]; then
      echo "ERROR: FPCLAZUP_URL not set" >&2
      exit 2
    fi
    echo "Downloading fpclazup: $FPCLAZUP_URL"
    curl -sSfL --retry 3 -o "$FPCLAZUP_BIN" "$FPCLAZUP_URL"
    chmod +x "$FPCLAZUP_BIN"
  fi

  ARGS=(--only=FPCCleanOnly,FPCBuildOnly --cputarget="$CPU" --ostarget="$OS"
        --noconfirm --verbose --installdir="$INSTALL_DIR")
  if [[ -n "$CROSS_BIN_DIR" ]]; then
    ARGS+=(--crossbindir="$CROSS_BIN_DIR")
  else
    ARGS+=(--autotools)
  fi
  # Presets of fpcupdeluxe GUI (like -Cb- -Caelfv2 for powerpc64-linux)
  # are not used by fpclazup, options must be passed explicitly
  if [[ -n "$CROSS_OPT" ]]; then
    ARGS+=(--crossOPT="$CROSS_OPT")
  fi
  # Libraries downloaded by --autotools are not found by fpclazup on Linux
  # hosts (the archive is extracted into a nested cross/lib/<target>/cross/lib/<target>
  # directory), so use the ones from the system
  if [[ -n "$CROSS_LIB_DIR" ]]; then
    ARGS+=(--crosslibdir="$CROSS_LIB_DIR")
  fi

  echo "Building FPC cross compiler for $CPU-$OS ($MARKER_TEXT)"
  for ATTEMPT in 1 2 3; do
    if "$FPCLAZUP_BIN" "${ARGS[@]}"; then
      break
    fi
    if [[ $ATTEMPT == 3 ]]; then
      exit 1
    fi
    echo "fpclazup failed, attempt $ATTEMPT of 3, trying again in 30 s"
    sleep 30
  done
  echo "$MARKER_TEXT" > "$MARKER"
fi

# shellcheck disable=SC2086 # options are separate words
INFO=$("$FPC" -P"$CPU" -T"$OS" $CROSS_OPT -iVTPTO)
echo "FPC cross compiler: $INFO"
