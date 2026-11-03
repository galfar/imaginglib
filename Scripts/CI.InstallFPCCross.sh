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

# Cached FPC installation may already have the cross compiler
# (cache is saved at the end of the job that built it)
if [[ "$PPCROSS" != "ppcross" ]] && [[ -n "$(find "$INSTALL_DIR" -type f -name "$PPCROSS" -print -quit)" ]]; then
  echo "Cross compiler $PPCROSS for $CPU-$OS already installed"
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

  echo "Building FPC cross compiler for $CPU-$OS"
  "$FPCLAZUP_BIN" \
    --only=FPCCleanOnly,FPCBuildOnly \
    --cputarget="$CPU" \
    --ostarget="$OS" \
    --autotools \
    --noconfirm \
    --verbose \
    --installdir="$INSTALL_DIR"
fi

echo "FPC cross compiler:" $(fpc -P"$CPU" -T"$OS" -iVTPTO)
