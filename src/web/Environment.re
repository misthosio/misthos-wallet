type t = {
  redirectURI: string,
  manifestURI: string,
  appDomain: string,
  webDomain: string,
  cookieDomain: string,
  network: Network.t,
  monitoringEnvironment: string,
};

let get = () => {
  let hostname =
    try (Location.hostname) {
    | _ => "server"
    };
  switch (hostname) {
  | "server" => {
      redirectURI: "",
      manifestURI: "",
      appDomain: "",
      webDomain: "",
      cookieDomain: "",
      network: Testnet,
      monitoringEnvironment: "web",
    }
  | "localhost" => {
      redirectURI: "http://localhost:3000/",
      manifestURI: "http://localhost:3000/manifest.json",
      appDomain: "http://localhost:3000",
      webDomain: "http://localhost:3001",
      cookieDomain: "localhost",
      network: Testnet,
      monitoringEnvironment: "dev",
    }
  | "staging.misthos.io"
  | "web-staging.misthos.io" => {
      redirectURI: "https://staging.misthos.io/",
      manifestURI: "https://staging.misthos.io/manifest.json",
      appDomain: "https://staging.misthos.io",
      webDomain: "https://web-staging.misthos.io",
      cookieDomain: "misthos.io",
      network: Testnet,
      monitoringEnvironment: "staging",
    }
  | "testnet.misthos.io"
  | "web-testnet.misthos.io" => {
      redirectURI: "https://testnet.misthos.io/",
      manifestURI: "https://testnet.misthos.io/manifest.json",
      appDomain: "https://testnet.misthos.io",
      webDomain: "https://web-testnet.misthos.io",
      cookieDomain: "misthos.io",
      network: Testnet,
      monitoringEnvironment: "testnet",
    }
  | "www.misthos.io"
  | "app.misthos.io" => {
      redirectURI: "https://app.misthos.io/",
      manifestURI: "https://app.misthos.io/manifest.json",
      appDomain: "https://app.misthos.io",
      webDomain: "https://www.misthos.io",
      cookieDomain: "misthos.io",
      network: Mainnet,
      monitoringEnvironment: "mainnet",
    }
  | _ => {
      redirectURI: Location.origin ++ "/",
      manifestURI: Location.origin ++ "/manifest.json",
      appDomain: Location.origin,
      webDomain: Location.origin,
      cookieDomain: "misthos.io",
      network: Testnet,
      monitoringEnvironment: "unknown",
    }
  };
};
