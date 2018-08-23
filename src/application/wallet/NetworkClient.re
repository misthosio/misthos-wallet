open WalletTypes;

module WithFalleback =
       (ClientA: NetworkClientInterface, ClientB: NetworkClientInterface) => {
  let network = ClientA.network;
  let getUTXOs = addresses =>
    Belt.(
      Js.Promise.(
        ClientA.getUTXOs(addresses)
        |> then_(utxos =>
             ClientB.getUTXOs(addresses)
             |> then_(moreUtxos =>
                  List.concat(utxos, moreUtxos)
                  |. List.reduceU(
                       (Set.String.empty, []),
                       (. (known, res), {txId} as utxo: WalletTypes.utxo) =>
                       known |. Set.String.has(txId) ?
                         (known, res) :
                         (known |. Set.String.add(txId), [utxo, ...res])
                     )
                  |> snd
                  |> resolve
                )
             |> catch(_ => utxos |> resolve)
           )
        |> catch(_ => ClientB.getUTXOs(addresses))
      )
    );
  let getTransactionInfo = txIds =>
    Js.Promise.(
      ClientA.getTransactionInfo(txIds)
      |> then_(txInfos =>
           txInfos |> Belt.List.size != (txIds |> Belt.Set.String.size) ?
             ClientB.getTransactionInfo(txIds) : txInfos |> resolve
         )
      |> catch(_ => ClientB.getTransactionInfo(txIds))
    );
  let getTransactionHex = txIds =>
    Js.Promise.(
      ClientA.getTransactionHex(txIds)
      |> then_(txHex =>
           txHex |> Belt.Array.size != (txIds |> Belt.Array.size) ?
             ClientB.getTransactionHex(txIds) : txHex |> resolve
         )
      |> catch(_ => ClientB.getTransactionHex(txIds))
    );
  let getCurrentBlockHeight = () =>
    Js.Promise.(
      ClientA.getCurrentBlockHeight()
      |> catch(_ => ClientB.getCurrentBlockHeight())
    );
  let broadcastTransaction = tx =>
    Js.Promise.(
      ClientA.broadcastTransaction(tx)
      |> then_(
           fun
           | Ok(id) => Ok(id) |> resolve
           | _ => ClientB.broadcastTransaction(tx),
         )
      |> catch(_ => ClientB.broadcastTransaction(tx))
    );
};

module Make = (Client: NetworkClientInterface) => {
  let network = Client.network;
  let transactionInfo = Client.getTransactionInfo;
  let transactionHex = Client.getTransactionHex;
  let currentBlockHeight = Client.getCurrentBlockHeight;
  let transactionInputs = addresses =>
    Belt.(
      Js.Promise.(
        addresses
        |> Map.String.keysToArray
        |> List.fromArray
        |> Client.getUTXOs
        |> then_(utxos =>
             utxos
             |. List.map(({txId, txOutputN, address, amount}: utxo) =>
                  (
                    {
                      let a: Address.t =
                        addresses
                        |. Map.String.get(address)
                        |> Js.Option.getExn;
                      {
                        txId,
                        txOutputN,
                        address,
                        nCoSigners: a.nCoSigners,
                        nPubKeys: a.nPubKeys,
                        value: amount,
                        coordinates: a.coordinates,
                        sequence: a.sequence,
                        unlocked: false,
                      };
                    }: Network.txInput
                  )
                )
             |> resolve
           )
      )
    );
  let broadcastTransaction = Client.broadcastTransaction;
};

module Regtest =
  Make(
    (
      val BitcoindClient.make(
            {
              bitcoindUrl: "http://localhost:18322",
              rpcUser: "bitcoin",
              rpcPassword: "bitcoin",
            }: BitcoindClient.config,
            Bitcoin.Networks.testnet,
          )
    ),
  );

module Testnet =
  Make(
    (
      WithFalleback(
        (
          val BlockchainInfoClient.make(
                BlockchainInfoClient.testnetConfig,
                Bitcoin.Networks.testnet,
              )
        ),
        (
          val SmartbitClient.make(
                SmartbitClient.testnetConfig,
                Bitcoin.Networks.testnet,
              )
        ),
      )
    ),
  );

module Mainnet =
  Make(
    (
      WithFalleback(
        (
          val BlockchainInfoClient.make(
                BlockchainInfoClient.mainnetConfig,
                Bitcoin.Networks.bitcoin,
              )
        ),
        (
          val SmartbitClient.make(
                SmartbitClient.mainnetConfig,
                Bitcoin.Networks.bitcoin,
              )
        ),
      )
    ),
  );

let transactionInputs =
  fun
  | Network.Regtest => Regtest.transactionInputs
  | Network.Testnet => Testnet.transactionInputs
  | Network.Mainnet => Mainnet.transactionInputs;

let transactionInfo =
  fun
  | Network.Regtest => Regtest.transactionInfo
  | Network.Testnet => Testnet.transactionInfo
  | Network.Mainnet => Mainnet.transactionInfo;

let transactionHex =
  fun
  | Network.Regtest => Regtest.transactionHex
  | Network.Testnet => Testnet.transactionHex
  | Network.Mainnet => Mainnet.transactionHex;

let currentBlockHeight =
  fun
  | Network.Regtest => Regtest.currentBlockHeight
  | Network.Testnet => Testnet.currentBlockHeight
  | Network.Mainnet => Mainnet.currentBlockHeight;

let broadcastTransaction =
  fun
  | Network.Regtest => Regtest.broadcastTransaction
  | Network.Testnet => Testnet.broadcastTransaction
  | Network.Mainnet => Mainnet.broadcastTransaction;
