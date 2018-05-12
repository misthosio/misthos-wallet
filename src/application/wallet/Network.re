open WalletTypes;

type t =
  | Regtest
  | Testnet
  | Mainnet;

let encode =
  fun
  | Regtest => Json.Encode.string("regtest")
  | Testnet => Json.Encode.string("testnet")
  | Mainnet => Json.Encode.string("mainnet");

let decode = raw => {
  let name = raw |> Json.Decode.string;
  switch (name) {
  | "regtest" => Regtest
  | "testnet" => Testnet
  | "mainnet" => Mainnet
  | _ => %assert
         "Network.decode"
  };
};

type txInput = {
  txId: string,
  txOutputN: int,
  address: string,
  value: BTC.t,
  nCoSigners: int,
  nPubKeys: int,
  coordinates: Address.Coordinates.t,
};

let encodeInput = input =>
  Json.Encode.(
    object_([
      ("txId", string(input.txId)),
      ("txOutputN", int(input.txOutputN)),
      ("address", string(input.address)),
      ("value", BTC.encode(input.value)),
      ("nCoSigners", int(input.nCoSigners)),
      ("nPubKeys", int(input.nPubKeys)),
      ("coordinates", Address.Coordinates.encode(input.coordinates)),
    ])
  );

let decodeInput = raw =>
  Json.Decode.{
    txId: raw |> field("txId", string),
    txOutputN: raw |> field("txOutputN", int),
    address: raw |> field("address", string),
    value: raw |> field("value", BTC.decode),
    nCoSigners: raw |> field("nCoSigners", int),
    nPubKeys: raw |> field("nPubKeys", int),
    coordinates: raw |> field("coordinates", Address.Coordinates.decode),
  };

module Make = (Client: NetworkClient) => {
  let network = Client.network;
  let transactionInputs = (coordinates, accountKeyChains) => {
    let addresses =
      coordinates
      |> List.map(c => {
           let address = Address.find(c, accountKeyChains);
           (address.address, (c, address));
         });
    Js.Promise.(
      Client.getUTXOs(addresses |> List.map(fst))
      |> then_(utxos =>
           utxos
           |> List.map(({txId, txOutputN, address, amount}: utxo) =>
                {
                  txId,
                  txOutputN,
                  address,
                  nCoSigners:
                    snd(addresses |> List.assoc(address)).nCoSigners,
                  nPubKeys: snd(addresses |> List.assoc(address)).nPubKeys,
                  value: amount,
                  coordinates: addresses |> List.assoc(address) |> fst,
                }
              )
           |> resolve
         )
    );
  };
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
      val SmartbitClient.make(
            SmartbitClient.testnetConfig,
            Bitcoin.Networks.testnet,
          )
    ),
  );

module Mainnet =
  Make(
    (
      val SmartbitClient.make(
            SmartbitClient.mainnetConfig,
            Bitcoin.Networks.bitcoin,
          )
    ),
  );

let transactionInputs =
  fun
  | Regtest => Regtest.transactionInputs
  | Testnet => Testnet.transactionInputs
  | Mainnet => Mainnet.transactionInputs;

let broadcastTransaction =
  fun
  | Regtest => Regtest.broadcastTransaction
  | Testnet => Testnet.broadcastTransaction
  | Mainnet => Mainnet.broadcastTransaction;

let bitcoinNetwork =
  fun
  | Regtest => Regtest.network
  | Testnet => Testnet.network
  | Mainnet => Mainnet.network;

/* For now send income to faucet */
let regtestIncomeAddress = "2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF";

let testnetIncomeAddress = "2NFTHU1CBjt9bW8PJjkZ8AfMx8raJoeupx5";

let incomeAddress =
  fun
  | Regtest => regtestIncomeAddress
  | Testnet => testnetIncomeAddress
  | Mainnet => testnetIncomeAddress;
