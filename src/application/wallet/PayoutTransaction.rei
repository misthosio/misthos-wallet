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

type summary = {
  reserved: BTC.t,
  destinations: list((string, BTC.t)),
  spentWithFees: BTC.t,
  misthosFee: BTC.t,
  networkFee: BTC.t,
};

let summary: (Network.t, t) => summary;

let collide: (t, t) => bool;

let txInputForChangeAddress: (~txId: string, Network.t, t) => option(input);

let build:
  (
    ~mandatoryInputs: Network.inputSet,
    ~allInputs: Network.inputSet,
    ~destinations: list((string, BTC.t)),
    ~satsPerByte: BTC.t,
    ~changeAddress: Address.t,
    ~network: Network.t
  ) =>
  t;

let max:
  (
    ~allInputs: Network.inputSet,
    ~targetDestination: string,
    ~destinations: list((string, BTC.t)),
    ~satsPerByte: BTC.t,
    ~network: Network.t
  ) =>
  BTC.t;

type signResult =
  | Signed(t)
  | NotSigned;

let getSignedExn: signResult => t;

let signPayout:
  (
    ~ventureId: ventureId,
    ~userId: userId,
    ~masterKeyChain: Bitcoin.HDNode.t,
    ~accountKeyChains: AccountKeyChain.Collection.t,
    ~payoutTx: t,
    ~network: Network.t
  ) =>
  signResult;

let finalize: (list(t), Network.t) => Bitcoin.Transaction.t;

let encode: t => Js.Json.t;

let decode: Js.Json.t => t;
