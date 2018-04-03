open PrimitiveTypes;

open WalletTypes;

exception NotEnoughFunds;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: list((int, input))
};

let build:
  (
    ~mandatoryInputs: list(input),
    ~allInputs: list(input),
    ~destinations: list((string, BTC.t)),
    ~satsPerByte: BTC.t,
    ~changeAddress: AccountKeyChain.Address.t,
    ~network: Bitcoin.Networks.t
  ) =>
  (t, bool);

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
    ~payout: t,
    ~network: Bitcoin.Networks.t
  ) =>
  t;
