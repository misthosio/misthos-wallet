#!/bin/bash

set -e

cd repo
make install
make ci
