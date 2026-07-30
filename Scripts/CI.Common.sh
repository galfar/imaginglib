#!/usr/bin/env bash
set -e

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