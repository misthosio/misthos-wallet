#!/bin/bash

set -e

tar -zxvf misthos-code/misthos-code-*.tgz > /dev/null

cp pipeline-tasks/Makefile repo
cp -r pipeline-tasks/scripts repo

pushd repo

export GENERATE_SOURCEMAP='false'
make build

popd

tar -zcvf "misthos-build-v$(cat build-version/number).tgz" repo/dist
mv ./*.tgz build

