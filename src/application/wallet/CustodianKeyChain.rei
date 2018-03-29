open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t;

type public;

let make:
  (
    ~ventureId: ventureId,
    ~accountIdx: accountIdx,
    ~keyChainIdx: custodianKeyChainIdx,
    ~masterKeyChain: HDNode.t
  ) =>
  t;

let toPublicKeyChain: t => public;

let accountIdx: public => accountIdx;

let keyChainIdx: public => custodianKeyChainIdx;

let hdNode: public => HDNode.t;

let encode: public => Js.Json.t;

let decode: Js.Json.t => public;
