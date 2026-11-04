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
# Cross binutils and libraries are downloaded by fpclazup (--autotools).

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
# Records options the installed cross compiler and its RTL were built with
MARKER="$INSTALL_DIR/ci-cross-$CPU-$OS.txt"
MARKER_TEXT="crossopt=$CROSS_OPT"

# Cached FPC installation may already have the cross compiler (cache is saved
# at the end of the job that built it). Reused only when it was built by this
# script with the same options.
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
    curl -sSfL -o "$FPCLAZUP_BIN" "$FPCLAZUP_URL"
    chmod +x "$FPCLAZUP_BIN"
  fi

  ARGS=(--only=FPCCleanOnly,FPCBuildOnly --cputarget="$CPU" --ostarget="$OS"
        --autotools --noconfirm --verbose --installdir="$INSTALL_DIR")
  # Presets of fpcupdeluxe GUI (like -Cb- -Caelfv2 for powerpc64-linux)
  # are not used by fpclazup, options must be passed explicitly
  if [[ -n "$CROSS_OPT" ]]; then
    ARGS+=(--crossOPT="$CROSS_OPT")
  fi

  echo "Building FPC cross compiler for $CPU-$OS ($MARKER_TEXT)"
  "$FPCLAZUP_BIN" "${ARGS[@]}"
  echo "$MARKER_TEXT" > "$MARKER"
fi

# shellcheck disable=SC2086 # options are separate words
INFO=$("$FPC" -P"$CPU" -T"$OS" $CROSS_OPT -iVTPTO)
echo "FPC cross compiler: $INFO"
