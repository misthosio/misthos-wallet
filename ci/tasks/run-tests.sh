cp -r deps/* repo

#!/bin/bash

set -e

pushd repo

make install
make ci

popd

cp -r repo/node_modules deps
