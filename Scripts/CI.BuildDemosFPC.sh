#!/usr/bin/env bash
set -eo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

# FPC build scripts are expected to from the Scripts directory
cd "$REPO_ROOT/Scripts"

./BuildDemosFPC.sh

# TODO: Needs updated SDL and OpenGL headers, skipping for now on ARM64
if [ "$RUNNER_ARCH" != "ARM64" ]; then
  ./BuildExtDemosFPC.sh
fi

./Clean.sh --clean-also-bin-dirs
