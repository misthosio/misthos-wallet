#!/bin/bash

set -e

pushd deps
make install
git log --pretty=format:'%h' -n 1 > gitref
popd

tar -zcvf "bundled-deps-v$(cat deps-version/number)-$(cat deps/gitref).tgz" deps > /dev/null

mv ./*.tgz bundled-deps
