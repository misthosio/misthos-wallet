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
  sequence: option(int),
  unlocked: bool,
};

module TxInputCmp = {
  let compareInputs =
    (.
      {txId: id1, txOutputN: out1}: txInput,
      {txId: id2, txOutputN: out2}: txInput,
    ) => {
      let c = compare(id1, id2);
      if (c != 0) {
        c;
      } else {
        compare(out1, out2);
      };
    };
  include Belt.Id.MakeComparableU({
    type t = txInput;
    let cmp = compareInputs;
  });
};

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
      ("sequence", nullable(int, input.sequence)),
      ("unlocked", nullable(bool, input.unlocked ? Some(true) : None)),
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
    sequence: raw |> optional(field("sequence", int)),
    unlocked: raw |> optional(field("unlocked", bool)) |> Js.Option.isSome,
  };

let bitcoinNetwork =
  fun
  | Regtest => Bitcoin.Networks.testnet

  | Testnet => Bitcoin.Networks.testnet

  | Mainnet => Bitcoin.Networks.bitcoin;

let testnetIncomeAddress = "2Mt1spz31MXtY5bVHUAEGtFQcFHrG9gza6k";

let incomeAddress =
  fun
  | Regtest => testnetIncomeAddress
  | Testnet => testnetIncomeAddress
  | Mainnet => "3LSfn8iuxcy9Ty1D7te1DtiiLR6nekgHPm";
