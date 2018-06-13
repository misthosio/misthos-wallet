type t = {
  redirectURI: string,
  manifestURI: string,
  appDomain: string,
  cookieDomain: string,
  network: Network.t,
};

let get = () => {
  let default = {
    redirectURI: Location.origin ++ "/",
    manifestURI: Location.origin ++ "/manifest.json",
    appDomain: Location.origin,
    cookieDomain: "misthos.io",
    network: Testnet,
  };
  switch (Location.hostname) {
  | "localhost" => {
      redirectURI: "http://localhost:3000/",
      manifestURI: "http://localhost:3000/manifest.json",
      appDomain: "http://localhost:3000",
      cookieDomain: "localhost",
      network: Testnet,
    }
  | "web-staging.misthos.io" => {
      redirectURI: "https://staging.misthos.io/",
      manifestURI: "https://staging.misthos.io/manifest.json",
      appDomain: "https://staging.misthos.io",
      cookieDomain: "misthos.io",
      network: Testnet,
    }
  | "web-testnet.misthos.io" => {
      redirectURI: "https://testnet.misthos.io/",
      manifestURI: "https://testnet.misthos.io/manifest.json",
      appDomain: "https://testnet.misthos.io",
      cookieDomain: "misthos.io",
      network: Testnet,
    }
  | "app.misthos.io" => {...default, network: Mainnet}
  | _ => default
  };
};
