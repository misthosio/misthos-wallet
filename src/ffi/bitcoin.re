module BigInteger = {
  type t;
  [@bs.module "bigi"] external fromHex : string => t = "";
};

module Crypto = {
  [@bs.module "bitcoinjs-lib"] [@bs.scope "crypto"]
  external sha256 : string => Node.buffer = "";
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

module ECSignature = {
  type t;
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECSignature"]
  external fromDER : Node.buffer => t = "";
  [@bs.send] external toDER : t => Node.buffer = "";
};

module ECPair = {
  type t;
  type options = {. "network": Networks.t};
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external makeRandom : unit => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external makeRandomWithOptions : options => t = "makeRandom";
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

module Tx = {
  type t;
  [@bs.send] external toHex : t => string = "";
};

module TxBuilder = {
  type t;
  [@bs.new] [@bs.module "bitcoinjs-lib"]
  external create : unit => t = "TransactionBuilder";
  [@bs.new] [@bs.module "bitcoinjs-lib"]
  external createWithOptions :
    (~network: Networks.t=?, ~maxixumFeeRate: int=?, unit) => t =
    "TransactionBuilder";
  [@bs.send] external addInput : (t, string, int) => int = "";
  [@bs.send] external addOutput : (t, string, float) => int = "";
  [@bs.send] external sign : (t, int, ECPair.t) => unit = "";
  [@bs.send] external build : t => Tx.t = "";
};
