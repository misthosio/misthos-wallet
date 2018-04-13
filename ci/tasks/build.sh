#!/bin/bash

cp -r deps/* repo

set -e

pushd repo

make install

export GENERATE_SOURCEMAP='false'
make build

popd

cp -r repo/node_modules deps
cp -r repo/build/* build

