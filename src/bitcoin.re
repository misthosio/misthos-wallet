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
  external networks_ : {. "bitcoin": t, "testnet": t, "litecoin": t} =
    "networks";
  let bitcoin = networks_##bitcoin;
  let testnet = networks_##testnet;
  let litecoin = networks_##litecoin;
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
  [@bs.send] external toWIF_ : t => string = "toWIF";
  [@bs.send] external getAddress_ : t => string = "getAddress";
  let makeRandom: unit => t = () => makeRandom_(ecpair_);
  let fromWIF = (wif, networks) =>
    switch (Array.length(networks)) {
    | 0 => fromWIF_(ecpair_, wif, `Single(None))
    | _ => fromWIF_(ecpair_, wif, `Array(networks))
    };
  let toWIF: t => string = (ecpair) => toWIF_(ecpair);
  let getAddress: t => string = (ecpair) => getAddress_(ecpair);
};
