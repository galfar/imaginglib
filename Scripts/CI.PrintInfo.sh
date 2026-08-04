#!/usr/bin/env bash
set -eo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

echo "Lazarus:" $(lazbuild --version)
echo "FPC:" $(fpc -iWTPTO)
echo "OS: "$RUNNER_OS", ARCH:" $RUNNER_ARCH
if [ "$RUNNER_OS" == "Linux" ]; then
echo "    "$(lsb_release -d)
fi
