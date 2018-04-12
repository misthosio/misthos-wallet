#!/bin/bash

cp -r deps/* repo

set -e

cd repo
make install
cd ../
cp -r repo/* repo-with-deps/
rm -rf deps/*
cp -r repo/node_modules deps
