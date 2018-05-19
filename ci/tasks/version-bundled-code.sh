#!/bin/bash

set -e

mv bundled-code/*.tgz "bundled-code-with-version/misthos-code-v$(cat version/number).tgz"
