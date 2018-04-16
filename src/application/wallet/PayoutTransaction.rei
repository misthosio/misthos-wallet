open PrimitiveTypes;

open WalletTypes;

exception NotEnoughFunds;

exception NotEnoughSignatures;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: list((int, input)),
  withChange: bool,
};

type summary = {
  reserved: BTC.t,
  spent: BTC.t,
  fee: BTC.t,
};

let summary: t => summary;

type buildResult =
  | WithChangeAddress(t)
  | WithoutChangeAddress(t);

let build:
  (
    ~mandatoryInputs: list(input),
    ~allInputs: list(input),
    ~destinations: list((string, BTC.t)),
    ~satsPerByte: BTC.t,
    ~changeAddress: AccountKeyChain.Address.t,
    ~network: Network.t
  ) =>
  buildResult;

type signResult =
  | Signed(t)
  | NotSigned;

let getSignedExn: signResult => t;

let signPayout:
  (
    ~ventureId: ventureId,
    ~userId: userId,
    ~masterKeyChain: Bitcoin.HDNode.t,
    ~accountKeyChains: list(
                         (
                           accountIdx,
                           list((accountKeyChainIdx, AccountKeyChain.t)),
                         ),
                       ),
    ~payoutTx: t,
    ~network: Network.t
  ) =>
  signResult;

let finalize: (list(t), Network.t) => Bitcoin.Transaction.t;

let encode: t => Js.Json.t;

let decode: Js.Json.t => t;
