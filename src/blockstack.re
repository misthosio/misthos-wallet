[@bs.module "blockstack"] external isUserSignedIn : unit => bool = "";

[@bs.module "blockstack"] external isSignInPending : unit => bool = "";

type userData = {
  .
  "username": Js.nullable(string),
  "appPrivateKey": string
};

[@bs.module "blockstack"] [@bs.return nullable]
external loadUserData : unit => option(userData) = "";

[@bs.module "blockstack"] external redirectToSignIn : unit => unit = "";

[@bs.module "blockstack"] external signUserOut : unit => unit = "";

[@bs.module "blockstack"]
external handlePendingSignIn : unit => Js.Promise.t(userData) = "";

[@bs.module "blockstack"]
external putFile : (string, string, Js.boolean) => Js.Promise.t(unit) = "";

[@bs.module "blockstack"]
external getFile : (string, Js.boolean) => Js.Promise.t(Js.nullable(string)) =
  "";

type gaiaConfig = {
  .
  "url_prefix": string,
  "address": string
};
