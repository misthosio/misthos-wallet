type txInput = {
  txId: string,
  txOutputN: int,
  address: string,
  nCoSigners: int,
  nPubKeys: int,
};

module TxInputCmp =
  Belt.Id.MakeComparableU(
    {
      type t = txInput;
      let cmp =
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
      ("nCoSigners", int(input.nCoSigners)),
      ("nPubKeys", int(input.nPubKeys)),
    ])
  );

let decodeInput = raw =>
  Json.Decode.{
    txId: raw |> field("txId", string),
    txOutputN: raw |> field("txOutputN", int),
    address: raw |> field("address", string),
    nCoSigners: raw |> field("nCoSigners", int),
    nPubKeys: raw |> field("nPubKeys", int),
  };

type t = {
  address: string,
  txId: string,
  txOutputN: int,
};

let encode = event =>
  Json.Encode.(
    object_([
      ("type", string("IncomeDetected")),
      ("address", string(event.address)),
      ("txId", string(event.txId)),
      ("txOutputN", int(event.txOutputN)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    address: raw |> field("address", string),
    txId: raw |> field("txId", string),
    txOutputN: raw |> field("txOutputN", int),
  };
