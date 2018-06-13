#!/bin/bash

set -e

cat <<EOF > account.json
${GCP_SERVICE_ACCOUNT}
EOF

WEB_PATH="";
if [[ "${WEB}" == "true" ]]; then
  WEB_PATH="web-";
fi


gcloud auth activate-service-account --key-file ./account.json

tar -zxvf misthos-${WEB_PATH}build/misthos-${WEB_PATH}build-*.tgz
gsutil -m cp -r repo/${WEB_PATH}dist/* "gs://${SUBDOMAIN}.misthos.io"
