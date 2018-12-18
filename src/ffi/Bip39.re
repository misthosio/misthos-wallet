module Wordlist = {
  type t;
  [@bs.val] [@bs.module "bip39"] [@bs.scope "wordlists"]
  external english: t = "";
};

[@bs.module "bip39"]
external entropyToMnemonic: (Node.buffer, Wordlist.t) => string = "";

[@bs.module "bip39"] external mnemonicToSeed: string => Node.buffer = "";
