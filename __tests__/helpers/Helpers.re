open Bitcoin;

type utxo = BitcoindClient.bitcoindUTXO;

let enableHttpRequests = () => [%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

let faucetKey =
  ECPair.fromWIFWithNetwork(
    "92Qba5hnyWSn5Ffcka56yMQauaWY6ZLd91Vzxbi4a9CCetaHtYj",
    Networks.testnet
  );

let bitcoindConfig: BitcoindClient.config = {
  bitcoindUrl: "http://localhost:18322",
  rpcUser: "bitcoin",
  rpcPassword: "bitcoin"
};

let defaultFee = BTC.fromSatoshis(600L);

let selectUTXOs = (utxos, totalAmount) => {
  let utxos =
    utxos
    |> List.filter(({confirmations}: utxo) => confirmations > 0)
    |> List.sort((u1: utxo, u2: utxo) =>
         u1.amount |> BTC.comparedTo(u2.amount)
       );
  utxos
  |> List.fold_left(
       ((result, total), utxo: utxo) =>
         if (total |> BTC.gt(totalAmount |> BTC.plus(defaultFee))) {
           (result, total);
         } else {
           ([utxo, ...result], total |> BTC.plus(utxo.amount));
         },
       ([], BTC.zero)
     );
};

exception FaucetEmpty;

let getUTXOs = address => BitcoindClient.getUTXOs(bitcoindConfig, address);

let broadcastTransaction = tx =>
  BitcoindClient.broadcastTransaction(bitcoindConfig, tx)
  |> Js.Promise.then_(result => {
       Node.Child_process.execSync(
         "bitcoin-cli -regtest -rpcuser=bitcoin -rpcpassword=bitcoin -rpcport=18322 generate 2",
         Node.Child_process.option(~encoding="utf8", ())
       )
       |> ignore;
       Js.Promise.resolve(result);
     });

let fundAddress = (address, values, utxos) => {
  let totalValues =
    values |> List.fold_left((n, v) => n |> BTC.plus(v), BTC.zero);
  let (inputs, totalIn) = selectUTXOs(utxos, totalValues);
  if (totalIn < totalValues) {
    raise(FaucetEmpty);
  };
  let txB = TxBuilder.createWithNetwork(Networks.testnet);
  inputs
  |> List.iter((utxo: utxo) =>
       txB |> TxBuilder.addInput(utxo.txId, utxo.txOutputN) |> ignore
     );
  values
  |> List.iter(v =>
       txB |> TxBuilder.addOutput(address, v |> BTC.toSatoshisFloat) |> ignore
     );
  let remainder = totalIn |> BTC.minus(totalValues) |> BTC.minus(defaultFee);
  txB
  |> TxBuilder.addOutput(
       faucetKey |> ECPair.getAddress,
       remainder |> BTC.toSatoshisFloat
     )
  |> ignore;
  inputs |> List.iteri((i, _utxo) => txB |> TxBuilder.sign(i, faucetKey));
  Js.Promise.(
    broadcastTransaction(txB |> TxBuilder.build)
    |> then_((_) => BitcoindClient.getUTXOs(bitcoindConfig, address))
  );
};

let faucet = (address, values) =>
  Js.Promise.(
    BitcoindClient.getUTXOs(bitcoindConfig, faucetKey |> ECPair.getAddress)
    |> then_(fundAddress(address, values))
  );
