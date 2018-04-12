#!/bin/bash
set -e

cd repo
make install
cd ../
cp -r repo/* repo-with-deps/
