external isUserSignedIn : unit => bool = "" [@@bs.module "blockstack"];

external isSignInPending : unit => bool = "" [@@bs.module "blockstack"];

type userData = {username: string};

external loadUserData : unit => option userData = "" [@@bs.module "blockstack"];

external redirectToSignIn : unit => unit = "" [@@bs.module "blockstack"];

let signUserOut () => [%bs.raw {|require('blockstack').signUserOut(window.location.origin)|}];

let handlePendingSignIn () => [%bs.raw
  {|require('blockstack').handlePendingSignIn().then(userData => { window.location = window.location.origin; })|}
];
