open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t;

type public;

let make:
  (
    ~ventureId: ventureId,
    ~accountIndex: accountIdx,
    ~keyChainIndex: custodianKeyChainIdx,
    ~masterKeyChain: HDNode.t
  ) =>
  t;

let toPublicKeyChain: t => public;

let accountIndex: public => accountIdx;

let keyChainIndex: public => custodianKeyChainIdx;

let hdNode: public => HDNode.t;

let encode: public => Js.Json.t;

let decode: Js.Json.t => public;
