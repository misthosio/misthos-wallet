open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t;

type public;

let misthosWalletPurposePath: string;

let makePathToBip45Root:
  (
    ~ventureId: ventureId,
    ~accountIdx: accountIdx,
    ~keyChainIdx: custodianKeyChainIdx,
    HDNode.t
  ) =>
  string;

let fromHardwareNode:
  (
    ~hardwareId: string,
    ~accountIdx: accountIdx,
    ~keyChainIdx: custodianKeyChainIdx,
    HDNode.t
  ) =>
  public;

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

let getSigningKey: (coSignerIdx, chainIdx, addressIdx, t) => ECPair.t;

let hdNode: public => HDNode.t;

let encode: public => Js.Json.t;

let decode: Js.Json.t => public;

let eq: (public, public) => bool;
