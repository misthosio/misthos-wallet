#!/bin/bash


set -e

tar -zxvf misthos-code/misthos-code-*.tgz > /dev/null

pushd repo

export GENERATE_SOURCEMAP='false'
make build

popd

tar -zcvf "misthos-build-v$(cat build-version/number).tgz" repo/build
mv ./*.tgz build

