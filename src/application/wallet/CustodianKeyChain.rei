open PrimitiveTypes;

open Bitcoin;

type t;

type public;

let make:
  (
    ~ventureId: ventureId,
    ~accountIndex: int,
    ~keyChainIndex: int,
    ~masterKeyChain: HDNode.t
  ) =>
  t;

let toPublicKeyChain: t => public;

let getAccountIndex: public => int;

let getKeyChainIndex: public => int;

let encode: public => Js.Json.t;

let decode: Js.Json.t => public;
