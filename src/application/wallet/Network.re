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

module TxInputCmp =
  Belt.Id.MakeComparableU(
    {
      type t = txInput;
      let cmp =
        (.
          {txId, txOutputN}: txInput,
          {txId: id2, txOutputN: out2}: txInput,
        ) =>
          compare(
            txId ++ string_of_int(txOutputN),
            id2 ++ string_of_int(out2),
          );
    },
  );

type inputSet = Belt.Set.t(TxInputCmp.t, TxInputCmp.identity);

let inputSet = () => Belt.Set.make(~id=(module TxInputCmp));

type inputMap('a) = Belt.Map.t(TxInputCmp.t, 'a, TxInputCmp.identity);

let inputMap = () => Belt.Map.make(~id=(module TxInputCmp));

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
  let transactionInfo = Client.getTransactionInfo;
  let transactionInputs = addresses =>
    Belt.(
      Js.Promise.(
        addresses
        |> Map.String.keysToArray
        |> List.fromArray
        |> Client.getUTXOs
        |> then_(utxos =>
             utxos
             |. List.map(({txId, txOutputN, address, amount}: utxo) => {
                  let a: Address.t =
                    addresses |. Map.String.get(address) |> Js.Option.getExn;
                  {
                    txId,
                    txOutputN,
                    address,
                    nCoSigners: a.nCoSigners,
                    nPubKeys: a.nPubKeys,
                    value: amount,
                    coordinates: a.coordinates,
                  };
                })
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

let transactionInfo =
  fun
  | Regtest => Regtest.transactionInfo
  | Testnet => Testnet.transactionInfo
  | Mainnet => Mainnet.transactionInfo;

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

let testnetIncomeAddress = "2N2jV12sWwYxmTXttjFuYLxJsgeqV9WsazV";

let incomeAddress =
  fun
  | Regtest => testnetIncomeAddress
  | Testnet => testnetIncomeAddress
  | Mainnet => testnetIncomeAddress;
