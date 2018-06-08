type t = {
  redirectURI: unit => string,
  manifestURI: unit => string,
  appDomain: unit => string,
  cookieDomain: unit => string,
};

let default = {
  redirectURI: () => Location.origin ++ "/",
  manifestURI: () => Location.origin ++ "/manifest.json",
  appDomain: () => Location.origin,
  cookieDomain: () => Location.hostname,
};
