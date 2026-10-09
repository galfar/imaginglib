#!/usr/bin/env bash
set -eo pipefail

# Setup for GitHub Actions Ubuntu runners.
# 2026-08-04: ubuntu-latest is Ubuntu 24.04

# Just get the -dev packages for file formats, they don't have funny version suffixes
# like libtiff5 vs libtiff6 in different distro versions
sudo apt install libtiff-dev -y
sudo apt install libwebp-dev -y

# Needed for SDL and GL demos
sudo apt install libsdl1.2-dev -y

# To build LCL demos with GTK3 backend.
sudo apt install libgtk-3-dev

# To build LCL demos with Qt5 backend. If you see any linking problems, rather
# install the latest libqt5pas release from https://github.com/davidbannon/libqt5pas/releases
sudo apt install libqt5pas-dev -y

# With current Lazarus + Ubuntu versions we need to get newer libqt6pas release
BASE_URL=https://github.com/davidbannon/libqt6pas/releases/download/v6.2.10

if [[ "$RUNNER_ARCH" == "X64" ]]; then
    wget $BASE_URL/libqt6pas6_6.2.10-1_amd64.deb
    sudo apt install ./libqt6pas6_6.2.10-1_amd64.deb -y
    wget $BASE_URL/libqt6pas6-dev_6.2.10-1_amd64.deb
    sudo apt install ./libqt6pas6-dev_6.2.10-1_amd64.deb -y
else
    wget $BASE_URL/libqt6pas6_6.2.10-1_arm64.deb
    sudo apt install ./libqt6pas6_6.2.10-1_arm64.deb -y
    wget $BASE_URL/libqt6pas6-dev_6.2.10-1_arm64.deb
    sudo apt install ./libqt6pas6-dev_6.2.10-1_arm64.deb -y
fi
