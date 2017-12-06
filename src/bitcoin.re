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

module ECPair = {
  type t;
  type ecpairMod;
  [@bs.val] [@bs.module "bitcoinjs-lib"] external ecpair_ : ecpairMod = "ECPair";
  [@bs.send] external makeRandom_ : ecpairMod => t = "makeRandom";
  [@bs.send]
  external fromWIF_ :
    (
      ecpairMod,
      string,
      [@bs.unwrap] [ | `Single(option(Networks.t)) | `Array(array(Networks.t))]
    ) =>
    t =
    "fromWIF";
  [@bs.send] external toWIF : t => string = "toWIF";
  [@bs.send] external getAddress : t => string = "getAddress";
  let makeRandom: unit => t = () => makeRandom_(ecpair_);
  let fromWIF = (wif, networks) =>
    switch (Array.length(networks)) {
    | 0 => fromWIF_(ecpair_, wif, `Single(None))
    | _ => fromWIF_(ecpair_, wif, `Array(networks))
    };
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
