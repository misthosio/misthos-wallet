open PrimitiveTypes;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: list((int, input)),
  misthosFeeAddress: string,
  changeAddress: option((string, Address.Coordinates.t)),
};

type summary = {
  reserved: BTC.t,
  spentWithFees: BTC.t,
  misthosFee: BTC.t,
  networkFee: BTC.t,
};

let summary: (Network.t, t) => summary;

let txInputForChangeAddress:
  (~transactionId: string, AccountKeyChain.Collection.t, Network.t, t) =>
  option(input);

let build:
  (
    ~mandatoryInputs: list(input),
    ~allInputs: list(input),
    ~destinations: list((string, BTC.t)),
    ~satsPerByte: BTC.t,
    ~changeAddress: Address.t,
    ~network: Network.t
  ) =>
  t;

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
