#!/bin/bash

set -e

pushd deps
# make install
git log --pretty=format:'%h' -n 1 > gitref
popd

tar -zcvf "bundled-deps-$(cat deps/gitref).tgz" deps

mv ./*.tgz repo-with-deps
