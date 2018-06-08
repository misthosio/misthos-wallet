open Environment;

let getEnvironment = () =>
  switch (Location.hostname) {
  | "localhost" => {
      redirectURI: () => "http://localhost:3000/",
      manifestURI: () => "http://localhost:3000/manifest.json",
      appDomain: () => "http://localhost:3000",
    }
  | "web-staging.misthos.io" => {
      redirectURI: () => "https://staging.misthos.io/",
      manifestURI: () => "https://staging.misthos.io/manifest.json",
      appDomain: () => "https://staging.misthos.io",
    }
  | "web-testnet.misthos.io" => {
      redirectURI: () => "https://testnet.misthos.io/",
      manifestURI: () => "https://testnet.misthos.io/manifest.json",
      appDomain: () => "https://testnet.misthos.io",
    }
  | _ => Environment.default
  };

let getCookieDomain = () =>
  switch (Location.hostname) {
  | "localhost" => "localhost"
  | "web-staging.misthos.io" => "misthos.io"
  | "web-testnet.misthos.io" => "misthos.io"
  | _ => Location.hostname
  };
