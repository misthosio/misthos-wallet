#!/bin/bash

BITCION_DIR="./scripts/.bitcoin"
BITCOIN_RPC_PORT=18322
alias btc="bitcoin-cli -regtest -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=${BITCOIN_RPC_PORT}"
