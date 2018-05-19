#!/bin/bash

set -e

cat <<EOF > account.json
${GCP_SERVICE_ACCOUNT}
EOF


gcloud auth activate-service-account --key-file ./account.json

mkdir out
tar -zxvf misthos-build/misthos-build-*.tgz -C out
ls out
gsutil -m cp -r out/build/* gs://testnet.misthos.io
