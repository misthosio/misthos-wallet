#!/bin/bash

set -e

tar -zxvf bundled-deps/bundled-deps-*.tgz > /dev/null

if [[ $(diff -q deps/yarn.lock repo/yarn.lock) ]]; then
  echo "Deps are not up to date!"
  exit 1
fi

cp -r deps/node_modules repo/

pushd repo

make ci
git log --pretty=format:'%h' -n 1 > gitref
popd

tar -zcvf "misthos-code-$(cat repo/gitref).tgz" repo/ > /dev/null

mv ./*.tgz bundled-code
