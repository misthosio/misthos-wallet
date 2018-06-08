#!/bin/bash


set -e

tar -zxvf misthos-code/misthos-code-*.tgz > /dev/null

cp pipeline-tasks/Makefile repo

pushd repo

make web-export

popd

tar -zcvf "misthos-web-build-v$(cat build-version/number).tgz" repo/web-dist
mv ./*.tgz web-build

