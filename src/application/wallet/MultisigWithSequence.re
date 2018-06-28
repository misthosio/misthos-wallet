open Belt;
open Bitcoin;

let encode = (m, pubkeys, sequence) => {
  let n = pubkeys |> Array.length;

  Script.compile(
    Ops.(
      Array.concatMany([|
        [|
          op_DEPTH,
          op_2,
          op_EQUAL,
          op_IF,
          Script.Number.encode(sequence),
          op_CHECKSEQUENCEVERIFY,
          op_DROP,
          op_1,
          op_ELSE,
          Ops.numbers(m),
          op_ENDIF,
        |],
        pubkeys,
        [|Ops.numbers(n), op_CHECKMULTISIG|],
      |])
    ),
  );
};
