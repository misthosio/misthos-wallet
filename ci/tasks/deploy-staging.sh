#!/bin/bash

set -e

cat <<EOF > account.json
${GCP_SERVICE_ACCOUNT}
EOF


gcloud auth activate-service-account --key-file ./account.json

tar -zxvf misthos-build/misthos-build-*.tgz
gsutil -m cp -r repo/build/* gs://testnet.misthos.io
