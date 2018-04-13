#!/bin/bash

set -e

cat <<EOF > account.json
${GCP_SERVICE_ACCOUNT}
EOF


gcloud auth activate-service-account --key-file ./account.json

gsutil -m cp -r build/* gs://testnet.misthos.io
