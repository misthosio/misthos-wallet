function logMessage(msg) {
  console.log("[Income Worker] - " + msg);
};

testnetApiEndpoint = "https://testnet-api.smartbit.com.au/v1/blockchain/address"

function fetchTransactionsForAddress(address) {
  return fetch(testnetApiEndpoint + "/" + address).then(response => {
    if (response.status == 200) {
      return response.json();
    } else {
      return null;
    }
  }).then(resultJson => {
    if (resultJson != null && resultJson.success) {
        return resultJson.address.transactions || [];
    } else {
      return [];
    }
  });
};

function transactionsContainingOutput(transactions, address) {
  return transactions.filter(t => {
    return t.outputs.findIndex(o => o.addresses.includes(address)) != -1;
  });
};

intervalId = -1;
addresses = [];
knownTxIds = [];
tenSecondsInMilliseconds = 10000;
syncInterval = tenSecondsInMilliseconds;

self.onmessage = ({data}) => {
  logMessage("Message received");

  addresses = data.toMonitor;
  knownTxIds = data.knownTxIds;

  logMessage("Scanning transactions");
  addresses.forEach(address => {
    fetchTransactionsForAddress(address).then(txs => {
      newTransactions = transactionsContainingOutput(txs, address).filter(tx => {
        return knownTxIds.includes(tx.txid) == false
      });
      if (newTransactions.length > 0) {
        postMessage(newTransactions);
      }
    })
  })

  if (intervalId != -1) {
    clearInterval(intervalId);
  }

  intervalId = setInterval(function(){
    logMessage("Scanning transactions");
    addresses.forEach(address => {
      fetchTransactionsForAddress(address).then(txs => {
        newTransactions = transactionsContainingOutput(txs, address).filter(tx => {
          return knownTxIds.includes(tx.txid) == false
        });
        if (newTransactions.length > 0) {
          postMessage(newTransactions);
        }
      })
    });
  }, syncInterval);
};
