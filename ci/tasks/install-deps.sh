#!/bin/bash
set -e

ls
cp -r deps/* repo
cd repo
make install
cd ../
cp -r repo/* repo-with-deps/
rm -rf deps/*
cp -r repo/node_modules deps
