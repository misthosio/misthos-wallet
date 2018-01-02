module Crypto: {let sha256: string => Node.buffer;};

module Networks: {
  type t = {
    .
    "messagePrefix": string,
    "bech32": string,
    "bip32": {. public: string, private: string},
    "pubKeyHash": string,
    "scriptHash": string,
    "wif": string
  };
  let bitcoin: t;
  let litecoin: t;
  let testnet: t;
};

module ECPair: {
  type t;
  let makeRandom: unit => t;
  let fromWIF: string => t;
  let toWIF: t => string;
  let getAddress: t => string;
};

module Tx: {type t; let toHex: t => string;};

module TxBuilder: {
  type t;
  let create: (~network: Networks.t=?, ~maxixumFeeRate: int=?, unit) => t;
  let addInput: (t, string, int) => int;
  let addOutput: (t, string, int) => int;
  let sign: (t, int, ECPair.t) => unit;
  let build: t => Tx.t;
};
