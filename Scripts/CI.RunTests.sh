#!/usr/bin/env bash
set -eo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

cd "$REPO_ROOT/Bin"

# Test program returns non-zero exit code when some test fails

printf '\n===== Unit tests =====\n'
./ImagingTests

if [ "$RUNNER_OS" == "Windows" ]; then
    printf '\n===== Unit tests (Win32) =====\n'
    ./ImagingTests32
fi
