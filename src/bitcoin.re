module Networks = {
  type t = {
    .
    "messagePrefix": string,
    "bech32": string,
    "bip32": {. public: string, private: string},
    "pubKeyHash": string,
    "scriptHash": string,
    "wif": string
  };
  [@bs.val] [@bs.module "bitcoinjs-lib"]
  external networks : {. "bitcoin": t, "testnet": t, "litecoin": t} =
    "networks";
  let bitcoin = networks##bitcoin;
  let testnet = networks##testnet;
  let litecoin = networks##litecoin;
};

module ECSignature = {
  type t;
  [@bs.send] external toDER : t => Buffer.t = "";
};

module ECPair = {
  type t;
  type ecpairMod;
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"] external makeRandom : unit => t = "";
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"] external fromWIF : string => t = "";
  /* Complete function incase needed */
  [@bs.module "bitcoinjs-lib"] [@bs.scope "ECPair"]
  external fromWIF_ :
    (string, [@bs.unwrap] [ | `Single(option(Networks.t)) | `Array(array(Networks.t))]) => t =
    "fromWIF";
  [@bs.send] external toWIF : t => string = "";
  [@bs.send] external getAddress : t => string = "";
};

module Tx = {
  type t;
  [@bs.send] external toHex : t => string = "";
};

module TxBuilder = {
  type t;
  [@bs.new] [@bs.module "bitcoinjs-lib"]
  external create : (~network: Networks.t=?, ~maxixumFeeRate: int=?, unit) => t =
    "TransactionBuilder";
  [@bs.send] external addInput : (t, string, int) => int = "";
  [@bs.send] external addOutput : (t, string, int) => int = "";
  [@bs.send] external sign : (t, int, ECPair.t) => unit = "";
  [@bs.send] external build : t => Tx.t = "";
};
