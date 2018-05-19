#!/bin/bash

cp -r deps/* repo

set -e

pushd repo

make install

export GENERATE_SOURCEMAP='false'
make build

popd

cp -r repo/node_modules deps
tar -zcvf "misthos-build-v$(cat version/number).tgz" repo/build
mv ./*.tgz build

