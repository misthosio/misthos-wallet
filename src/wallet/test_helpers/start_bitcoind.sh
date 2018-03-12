#!/bin/bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BITCOIN_DIR="${DIR}/.bitcoin"
BITCOIN_CONF="${BITCOIN_DIR}/bitcoin.conf"
BITCOIN_RPC_PORT=18322

if ps aux | grep bitcoin | grep daemon >/dev/null; then
  echo "Killing running daemon"
  kill "$(ps aux | grep bitcoin | grep daemon | awk '{ print $2 }')" >/dev/null 2>&1
fi

echo "Making bitcoin dir"
rm -rf ${BITCOIN_DIR}
mkdir ${BITCOIN_DIR}

echo "Writting bitcoin conf"
cat <<EOF > ${BITCOIN_CONF}
rpcuser=bitcoin
rpcpassword=bitcoin
regtest=1
txindex=1
listen=1
server=1
datadir=${BITCOIN_DIR}
debug=1
rpcserialversion=0
EOF

echo "Starting bitcoin daemon in regtest mode"
bitcoind -daemon -rpcport=${BITCOIN_RPC_PORT} -conf=${BITCOIN_CONF}

CMD="bitcoin-cli -regtest -datadir=${BITCOIN_DIR} -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=${BITCOIN_RPC_PORT}"

echo "Testing connection"
for i in `seq 1 10`;
do
  if [[ $(${CMD} -getinfo >/dev/null 2>&1) ]] ; then
    break
  else
    echo 'Waiting for connection to start'
    sleep 1
  fi
done
${CMD} -getinfo

echo "Generating blocks"
${CMD} generate 250
${CMD} generate 432

TEST_PRIVATE_KEY="92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj"
TEST_ADDRESS="mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU"

echo 'Importing privkey'
${CMD} importprivkey ${TEST_PRIVATE_KEY} "test"

echo "Sending to ${TEST_ADDRESS}"
${CMD} sendtoaddress ${TEST_ADDRESS} 100
${CMD} generate 18

${CMD} getbalance "test"
