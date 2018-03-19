module BigInteger = {
  type t;
  [@bs.module "bigi"] external fromHex : string => t = "";
};

module Crypto = {
  [@bs.module "bitcoinjs-lib"] [@bs.scope "crypto"]
  external sha256 : string => Node.buffer = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "crypto"]
  external sha256FromBuffer : Node.buffer => Node.buffer = "sha256";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "crypto"]
  external hash160 : Node.buffer => Node.buffer = "";
};

module Networks = {
  type t;
  [@bs.val] [@bs.module "bitcoinjs-lib"] [@bs.scope "networks"]
  external bitcoin : t = "";
  [@bs.val] [@bs.module "bitcoinjs-lib"] [@bs.scope "networks"]
  external testnet : t = "";
  [@bs.val] [@bs.module "bitcoinjs-lib"] [@bs.scope "networks"]
  external litecoin : t = "";
};

module Address = {
  [@bs.module "bitcoinjs-lib"] [@bs.scope "address"]
  external fromOutputScript : (Node.buffer, Networks.t) => string = "";
};

module ECSignature = {
  type t;
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECSignature"]
  external fromDER : Node.buffer => t = "";
  [@bs.send] external toDER : t => Node.buffer = "";
};

module ECPair = {
  type t;
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external makeRandom : unit => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external makeRandomWithOptions : {. "network": Networks.t} => t =
    "makeRandom";
  let makeRandomWithNetwork = network =>
    makeRandomWithOptions({"network": network});
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external fromPublicKeyBuffer : Node.buffer => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external fromWIF : string => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external fromWIFWithNetwork : (string, Networks.t) => t = "fromWIF";
  [@bs.module "bitcoinjs-lib"] [@bs.new]
  external create : BigInteger.t => t = "ECPair";
  [@bs.send] external toWIF : t => string = "";
  [@bs.send] external getAddress : t => string = "";
  [@bs.send] external getNetwork : t => Networks.t = "";
  [@bs.send] external getPublicKeyBuffer : t => Node.buffer = "";
  [@bs.send.pipe : t] external sign : Node.buffer => ECSignature.t = "";
  [@bs.send.pipe : t]
  external verify : (Node.buffer, ECSignature.t) => Js.boolean = "";
};

module Transaction = {
  type t;
  [@bs.send] external toHex : t => string = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "Transaction"]
  external fromHex : string => t = "";
};

module TxBuilder = {
  type signatures = array(Js.Nullable.t(Node.buffer));
  type input = {. "signatures": signatures};
  type t = {. "inputs": array(input)};
  [@bs.new] [@bs.module "bitcoinjs-lib"]
  external create : unit => t = "TransactionBuilder";
  [@bs.new] [@bs.module "bitcoinjs-lib"]
  external createWithNetwork : Networks.t => t = "TransactionBuilder";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "TransactionBuilder"]
  external fromTransaction : Transaction.t => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "TransactionBuilder"]
  external fromTransactionWithNetwork : (Transaction.t, Networks.t) => t =
    "fromTransaction";
  [@bs.send.pipe : t] external addInput : (string, int) => int = "";
  [@bs.send.pipe : t] external addOutput : (string, float) => int = "";
  [@bs.send.pipe : t] external sign : (int, ECPair.t) => unit = "";
  [@bs.send.pipe : t]
  external signSegwit :
    (
      int,
      ECPair.t,
      ~redeemScript: Node.buffer,
      [@bs.as {json|null|json}] _,
      ~witnessValue: float,
      ~witnessScript: Node.buffer
    ) =>
    unit =
    "sign";
  [@bs.send] external build : t => Transaction.t = "";
  [@bs.send] external buildIncomplete : t => Transaction.t = "";
};

module Script = {
  module Multisig = {
    module Output = {
      [@bs.module "bitcoinjs-lib"] [@bs.scope ("script", "multisig", "output")]
      external encode : (int, array(Node.buffer)) => Node.buffer = "";
    };
  };
  module ScriptHash = {
    module Output = {
      [@bs.module "bitcoinjs-lib"]
      [@bs.scope ("script", "scriptHash", "output")]
      external encode : Node.buffer => Node.buffer = "";
    };
  };
  module WitnessScriptHash = {
    module Output = {
      [@bs.module "bitcoinjs-lib"]
      [@bs.scope ("script", "witnessScriptHash", "output")]
      external encode : Node.buffer => Node.buffer = "";
    };
  };
  module NullData = {
    module Output = {
      [@bs.module "bitcoinjs-lib"] [@bs.scope ("script", "nullData", "output")]
      external encode : Node.buffer => string = "";
    };
  };
};
