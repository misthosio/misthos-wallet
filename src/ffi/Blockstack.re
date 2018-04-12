[@bs.module "blockstack"] external isUserSignedIn : unit => bool = "";

[@bs.module "blockstack"] external isSignInPending : unit => bool = "";

type userData = {
  .
  "username": Js.nullable(string),
  "appPrivateKey": string
};

[@bs.module "blockstack"] [@bs.return nullable]
external loadUserData : unit => option(userData) = "";

[@bs.module "blockstack"]
external redirectToSignIn :
  (~redirectURI: string, ~manifestURI: string, ~scopes: array(string)) => unit =
  "";

[@bs.module "blockstack"] external signUserOut : unit => unit = "";

[@bs.module "blockstack"]
external handlePendingSignIn : unit => Js.Promise.t(userData) = "";

[@bs.module "blockstack"]
external getFile : string => Js.Promise.t(Js.nullable(string)) = "";

[@bs.module "blockstack"]
external getFileDecrypted :
  (string, [@bs.as {json| {"decrypt": true} |json}] _) =>
  Js.Promise.t(Js.nullable(string)) =
  "getFile";

[@bs.module "blockstack"]
external getFileWithJsOpts :
  (string, Js.t({..})) => Js.Promise.t(Js.nullable(string)) =
  "getFile";

let getFileFromUser = (file, ~username) =>
  getFileWithJsOpts(file, {"username": username});

[@bs.module "blockstack"]
external putFile : (string, string) => Js.Promise.t(unit) = "";

[@bs.module "blockstack"]
external putFileEncrypted :
  (string, string, [@bs.as {json| {"encrypt": true} |json}] _) =>
  Js.Promise.t(unit) =
  "putFile";

[@bs.module "blockstack"]
external getUserAppFileUrl :
  (~path: string, ~username: string, ~appOrigin: string) =>
  Js.Promise.t(string) =
  "";
