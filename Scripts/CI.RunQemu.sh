#!/usr/bin/env bash
set -eo pipefail

# Runs cross-built (CI.BuildCross.sh) Linux unit tests and demos with QEMU user mode emulation.
#
# Usage: CI.RunQemu.sh <qemu-arch> <suffix> <sysroot>
#   e.g. CI.RunQemu.sh riscv64 riscv64 /usr/riscv64-linux-gnu
#   Needs qemu-user and the target libc (e.g. libc6-riscv64-cross package)
#   installed in <sysroot>, the executables link libc dynamically.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

QEMU_ARCH="$1"
SUFFIX="$2"
SYSROOT="$3"
if [[ -z "$QEMU_ARCH" || -z "$SUFFIX" || -z "$SYSROOT" ]]; then
  echo "Usage: $0 <qemu-arch> <suffix> <sysroot>" >&2
  exit 2
fi

QEMU="qemu-$QEMU_ARCH"
export QEMU_LD_PREFIX="$SYSROOT"

cd "$REPO_ROOT/Bin"
file "./ImagingTests-$SUFFIX" || true

# Test program returns non-zero exit code when some test fails
printf '\n===== Unit tests (%s) =====\n' "$SUFFIX"
$QEMU "./ImagingTests-$SUFFIX"

cd "$REPO_ROOT/Demos/Bin"

printf '\n===== Bench (%s) =====\n' "$SUFFIX"
$QEMU "./Bench-$SUFFIX"

printf '\n===== VampConvert (%s) =====\n' "$SUFFIX"
$QEMU "./VampConvert-$SUFFIX" -i=../Data/Tigers.jpg -o=./conv-Tigers-$SUFFIX.png -format=A8R8G8B8 -resize=1600x1400xlanczos -rotate=45
