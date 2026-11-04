#!/usr/bin/env bash
set -eo pipefail

# Directory containing CI.Common.sh, normally <repo>/Scripts
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

# Repository root, assuming Scripts is directly under it
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd -P)"

if [[ ! -f "$REPO_ROOT/Readme.md" ]] || [[ ! -d "$REPO_ROOT/Demos" ]]; then
  echo "ERROR: Unable to locate ImagingLib repository root: $REPO_ROOT" >&2
  exit 2
fi

# Auto-detect OS and ARCH if not running inside GitHub Actions (let's use the same values)
if [ -z "$RUNNER_OS" ]; then
  case "$(uname -s)" in
    Linux*)          RUNNER_OS=Linux ;;  # or Windows Subsystem for Linux (WSL)
    Darwin*)         RUNNER_OS=macOS ;;
    MINGW*|MSYS*|CYGWIN*) RUNNER_OS=Windows ;;
    *)               echo "ERROR: Cannot detect OS"; exit 1 ;;
  esac
  echo "RUNNER_OS not set, auto-detected: $RUNNER_OS"
fi

if [ -z "$RUNNER_ARCH" ]; then
  case "$(uname -m)" in
    x86_64|amd64)    RUNNER_ARCH=X64 ;;
    aarch64|arm64)   RUNNER_ARCH=ARM64 ;;
    *)               RUNNER_ARCH=$(uname -m) ;;
  esac
  echo "RUNNER_ARCH not set, auto-detected: $RUNNER_ARCH"
fi

# FPC command: fpc from PATH, or the fpc.sh wrapper of an fpclazup install
# (setup-lazarus in fpclazup mode puts only the Lazarus directory on PATH)
if [ -z "$FPC" ]; then
  FPC=fpc
  if ! command -v fpc > /dev/null && [ -n "$RUNNER_TEMP" ]; then
    for F in "$RUNNER_TEMP"/fpclazup/fpc/bin/*/fpc.sh; do
      if [ -x "$F" ]; then FPC="$F"; break; fi
    done
  fi
fi

# Prints FPC options a cross target needs, for both building its RTL and
# compiling programs (fpclazup does not put them into fpc.cfg).
# Usage: crossOptions <cpu> <os>
function crossOptions {
  if [[ "$1" == "powerpc64" && "$2" == "linux" ]]; then
    # Little endian ELFv2 (ppc64le), FPC cross compiler defaults to big endian ELFv1
    echo "-Cb- -Caelfv2"
  fi
}
