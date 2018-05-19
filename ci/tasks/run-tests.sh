#!/bin/bash

cp -r deps/* repo

set -e

pushd repo

make install
# make ci

popd

cp -r repo/node_modules deps
tar -zcvf "misthos-code.tgz" repo/ > /dev/null

mv ./*.tgz bundled-code
