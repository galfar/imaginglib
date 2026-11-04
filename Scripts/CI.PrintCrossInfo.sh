#!/usr/bin/env bash
set -eo pipefail

# Prints info about cross compiling toolchain: FPC cross compiler and options,
# binutils, target libc, and optionally QEMU used to run the executables.
# Run after CI.InstallFPCCross.sh.
#
# Usage: CI.PrintCrossInfo.sh <cpu> <os> [<qemu-arch> <sysroot>]
#   e.g. CI.PrintCrossInfo.sh riscv64 linux riscv64 /usr/riscv64-linux-gnu

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

CPU="$1"
OS="$2"
QEMU_ARCH="$3"
SYSROOT="$4"
if [[ -z "$CPU" || -z "$OS" ]]; then
  echo "Usage: $0 <cpu> <os> [<qemu-arch> <sysroot>]" >&2
  exit 2
fi

# Prints first line of a command output, or "not found"
function firstLine {
  "$@" 2>&1 | head -1 || true
}

CROSS_OPT="$(crossOptions "$CPU" "$OS")"
CROSS_LIB_DIR="$(crossLibDir "$CPU" "$OS")"

echo "Host:          $(uname -m), $(lsb_release -ds 2>/dev/null || uname -s)"
echo "Lazarus:       $(lazbuild --version 2>/dev/null | tail -1)"
# shellcheck disable=SC2086 # options are separate words
echo "FPC:           $("$FPC" -P"$CPU" -T"$OS" $CROSS_OPT -iVTPTO)"
echo "Cross options: ${CROSS_OPT:-none}"

if [[ -n "$RUNNER_TEMP" ]]; then
  PPCROSS=$(find "$RUNNER_TEMP/fpclazup/fpc/bin" -type f -name 'ppcross*' 2>/dev/null | sort | tr '\n' ' ')
  echo "Compilers:     ${PPCROSS:-none found}"
  # Binutils downloaded by fpclazup --autotools
  BIN_DIR="$RUNNER_TEMP/fpclazup/cross/bin/$CPU-$OS"
  if [[ -d "$BIN_DIR" ]]; then
    for TOOL in as ld; do
      FILE=$(find "$BIN_DIR" -maxdepth 1 \( -name "*-$TOOL" -o -name "wasm-$TOOL" \) -print -quit)
      if [[ -n "$FILE" ]]; then
        echo "Binutils $TOOL:   $(basename "$FILE"): $(firstLine "$FILE" --version)"
      fi
    done
  else
    echo "Binutils:      no $BIN_DIR"
  fi
fi

if [[ -n "$CROSS_LIB_DIR" ]]; then
  PKG=$(dpkg-query -S "$CROSS_LIB_DIR/libc.so" 2>/dev/null | cut -d: -f1 || true)
  VER=""
  if [[ -n "$PKG" ]]; then
    VER=$(dpkg-query -W -f='${Version}' "$PKG" 2>/dev/null || true)
  fi
  echo "Target libc:   $CROSS_LIB_DIR (${PKG:-unknown package} ${VER})"
fi

if [[ -n "$QEMU_ARCH" ]]; then
  echo "QEMU:          $(firstLine "qemu-$QEMU_ARCH" --version)"
  echo "QEMU sysroot:  ${SYSROOT:-none} (QEMU_LD_PREFIX)"
fi
