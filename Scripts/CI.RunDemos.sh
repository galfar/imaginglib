#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "$SCRIPT_DIR/CI.Common.sh"

cd "$REPO_ROOT/Demos/Bin"

printf '\n===== Bench =====\n'
./Bench

if [ "$RUNNER_OS" == "Windows" ]; then
    printf '\n===== Bench 32 =====\n'
    ./Bench32
fi

printf '\n===== VampConvert help =====\n'
./VampConvert -h
printf '\n===== VampConvert =====\n'
./VampConvert -i=../Data/Tigers.jpg -o=./conv-Tigers.png -format=A8R8G8B8 -resize=1600x1400xlanczos -rotate=45
