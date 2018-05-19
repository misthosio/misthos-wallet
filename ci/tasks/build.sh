#!/bin/bash

cp -r deps/* repo

set -e

pushd repo

make install

export GENERATE_SOURCEMAP='false'
# make build
mkdir build
echo dummy > build/dummy.txt

popd

cp -r repo/node_modules deps
tar -zcvf "misthos-build-$(cat version/number).tgz" build
mv ./*.tgz build

