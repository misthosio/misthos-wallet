#!/bin/bash


set -e

tar -zxvf misthos-code/misthos-code-*.tgz

pushd repo

export GENERATE_SOURCEMAP='false'
make build

popd

tar -zcvf "misthos-build-v$(cat version/number).tgz" repo/build
mv ./*.tgz build

