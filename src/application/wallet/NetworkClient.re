open WalletTypes;

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
      val BlockchainInfoClient.make(
            BlockchainInfoClient.testnetConfig,
            Bitcoin.Networks.testnet,
          )
    ),
  );

module Mainnet =
  Make(
    (
      val BlockchainInfoClient.make(
            BlockchainInfoClient.mainnetConfig,
            Bitcoin.Networks.bitcoin,
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
