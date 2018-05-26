open PrimitiveTypes;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

let misthosFeePercent: float;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: array(input),
  misthosFeeAddress: string,
  changeAddress: option(Address.t),
};

let txInputForChangeAddress: (~txId: string, Network.t, t) => option(input);

let encode: t => Js.Json.t;

let decode: Js.Json.t => t;
