#!/bin/bash

rm -rf dist/*
NODE_ENV=production node_modules/.bin/webpack --mode production
for file in "$(ls public | grep -v 'index.html')" ; do
  cp "public/$file" dist/;
done
