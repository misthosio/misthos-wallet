type t = {
  txId: string,
  txOutputN: int,
};

let encode = event =>
  Json.Encode.(
    object_([
      ("txId", string(event.txId)),
      ("txOutputN", int(event.txOutputN)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    txId: raw |> field("txId", string),
    txOutputN: raw |> field("txOutputN", int),
  };

module TxInputCmp =
  Belt.Id.MakeComparableU(
    {
      type nonrec t = t;
      let cmp =
        (. {txId: id1, txOutputN: out1}: t, {txId: id2, txOutputN: out2}: t) => {
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
