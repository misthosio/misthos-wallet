type privateKey = string;

type publicKey = string;

[@bs.module "blockstack"] external getPublicKeyFromPrivate : privateKey => publicKey = "";

[@bs.module "blockstack"] external publicKeyToAddress : publicKey => publicKey = "";
