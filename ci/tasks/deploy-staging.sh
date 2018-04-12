#!/bin/bash

set -e

cat <<EOF > account.json
${GCP_SERVICE_ACCOUNT}
EOF


gcloud auth activate-service-account --key-file ./account.json

gsutil cp -r build/* gs://test.misthos.io
