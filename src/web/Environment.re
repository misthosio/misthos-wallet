type t = {
  redirectURI: unit => string,
  manifestURI: unit => string,
  appDomain: unit => string,
  cookieDomain: unit => string,
};

let get = () =>
  switch (Location.hostname) {
  | "localhost" => {
      redirectURI: () => "http://localhost:3000/",
      manifestURI: () => "http://localhost:3000/manifest.json",
      appDomain: () => "http://localhost:3000",
      cookieDomain: () => "localhost",
    }
  | "web-staging.misthos.io" => {
      redirectURI: () => "https://staging.misthos.io/",
      manifestURI: () => "https://staging.misthos.io/manifest.json",
      appDomain: () => "https://staging.misthos.io",
      cookieDomain: () => "misthos.io",
    }
  | "web-testnet.misthos.io" => {
      redirectURI: () => "https://testnet.misthos.io/",
      manifestURI: () => "https://testnet.misthos.io/manifest.json",
      appDomain: () => "https://testnet.misthos.io",
      cookieDomain: () => "misthos.io",
    }
  | _ => {
      redirectURI: () => Location.origin ++ "/",
      manifestURI: () => Location.origin ++ "/manifest.json",
      appDomain: () => Location.origin,
      cookieDomain: () => "misthos.io",
    }
  };
