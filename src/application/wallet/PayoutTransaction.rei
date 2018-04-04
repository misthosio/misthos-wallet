open PrimitiveTypes;

open WalletTypes;

exception NotEnoughFunds;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: list((int, input))
};

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
    ~network: Bitcoin.Networks.t
  ) =>
  buildResult;

type signResult =
  | Signed(t)
  | NotSigned;

let signPayout:
  (
    ~ventureId: ventureId,
    ~session: Session.Data.t,
    ~accountKeyChains: list(
                         (
                           accountIdx,
                           list((accountKeyChainIdx, AccountKeyChain.t))
                         )
                       ),
    ~payoutTx: t,
    ~network: Bitcoin.Networks.t
  ) =>
  signResult;

let encode: t => Js.Json.t;

let decode: Js.Json.t => t;
