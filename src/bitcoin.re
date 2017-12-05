module ECPair = {
  type t;

  type ecpairMod = {. [@bs.meth] "makeRandom": unit => t};

  [@bs.val] [@bs.module "bitcoinjs-lib"] external ecpair : ecpairMod = "ECPair";

  let makeRandom: unit => t =
    () => ecpair##makeRandom();
  let hello = "shtoen";
};
