#!/bin/bash

cp -r deps/* repo

set -e

pushd repo

make install
make ci

popd

cp -r repo/node_modules deps
tar -zcvf "misthos-code.tgz" repo/

mv ./*.tgz bundled-code
