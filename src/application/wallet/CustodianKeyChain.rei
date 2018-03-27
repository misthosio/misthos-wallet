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

let accountIndex: public => int;

let keyChainIndex: public => int;

let hdNode: public => HDNode.t;

let encode: public => Js.Json.t;

let decode: Js.Json.t => public;
