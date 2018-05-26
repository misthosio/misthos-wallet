type input = {
  txId: string,
  txOutputN: int,
  address: string,
  value: BTC.t,
  nCoSigners: int,
  nPubKeys: int,
};

module InputCmp =
  Belt.Id.MakeComparableU(
    {
      type t = input;
      let cmp =
        (.
          {txId: id1, txOutputN: out1}: input,
          {txId: id2, txOutputN: out2}: input,
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

type inputSet = Belt.Set.t(InputCmp.t, InputCmp.identity);

let inputSet = () => Belt.Set.make(~id=(module InputCmp));
