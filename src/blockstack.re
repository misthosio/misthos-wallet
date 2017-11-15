[@bs.module "blockstack"] external isUserSignedIn : unit => bool = "";

[@bs.module "blockstack"] external isSignInPending : unit => bool = "";

type userData = {username: string};

[@bs.module "blockstack"] external loadUserData : unit => option(userData) = "";

[@bs.module "blockstack"] external redirectToSignIn : unit => unit = "";

let signUserOut = () => [%bs.raw {|require('blockstack').signUserOut(window.location.origin)|}];

let handlePendingSignIn = () => [%bs.raw
  {|require('blockstack').handlePendingSignIn().then(userData => { window.location = window.location.origin; })|}
];
