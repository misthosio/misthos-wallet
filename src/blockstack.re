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

type getFileOpts = {
  .
  "decrypt": Js.null(Js.boolean),
  "username": Js.null(string),
  "app": Js.null(string),
  "zoneFileLookupURL": Js.null(string)
};

[@bs.module "blockstack"]
external getFileWithJsOpts :
  (string, getFileOpts) => Js.Promise.t(Js.nullable(string)) =
  "getFile";

let getFileWithOpts =
    (file, ~decrypt=?, ~username=?, ~app=?, ~zoneFileLookupURL=?, ()) => {
  let opts = {
    "decrypt": Js.Null.from_opt(DoNotFormat.boolToJsBoolean(decrypt)),
    "username": Js.Null.from_opt(username),
    "app": Js.Null.from_opt(app),
    "zoneFileLookupURL": Js.Null.from_opt(zoneFileLookupURL)
  };
  getFileWithJsOpts(file, opts);
};

[@bs.module "blockstack"]
external putFile : (string, string) => Js.Promise.t(unit) = "";

[@bs.module "blockstack"]
external putFileEncrypted :
  (string, string, [@bs.as {json| {"encrypt": true} |json}] _) =>
  Js.Promise.t(unit) =
  "putFile";
