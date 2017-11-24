[@bs.module "blockstack"] external isUserSignedIn : unit => bool = "";

[@bs.module "blockstack"] external isSignInPending : unit => bool = "";

type userData = {. "username": Js.nullable(string)};

[@bs.module "blockstack"] [@bs.return nullable] external loadUserData : unit => option(userData) =
  "";

[@bs.module "blockstack"] external redirectToSignIn : unit => unit = "";

let signUserOut = () => [%bs.raw {|require('blockstack').signUserOut(window.location.origin)|}];

let handlePendingSignIn = () =>
  [%bs.raw
    {|require('blockstack').handlePendingSignIn().then(userData => { console.log("then handler"); window.location = window.location.origin; })|}
  ]
  |> ignore;

[@bs.module "blockstack"] external putFile : (string, string, Js.boolean) => Js.Promise.t(unit) =
  "";

[@bs.module "blockstack"] external getFile : (string, Js.boolean) => Js.Promise.t(string) = "";
