#!/bin/bash

rm -rf dist/*
NODE_ENV=production node_modules/.bin/webpack --mode production
for file in $(ls static | grep -v 'index.html') ; do
  cp "static/${file}" dist/;
done
