#! /usr/bin/env bash

set -euxo pipefail

# Build measurement tool.
rm -rf scripts/RAPL/build
cmake scripts/RAPL -B scripts/RAPL/build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build scripts/RAPL/build

# Perform benchmark.
python3 scripts/measure.py -o /root/data "$@"
