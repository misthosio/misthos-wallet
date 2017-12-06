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
  let fromWIF: (string, array(Networks.t)) => t;
  let toWIF: t => string;
  let getAddress: t => string;
};
