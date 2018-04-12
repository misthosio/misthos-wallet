#!/bin/bash

set -e

cd repo-with-deps
make build
cd ..
cp -r repo-with-deps/build build
ls build

