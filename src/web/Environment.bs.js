// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';


function get() {
  var default_000 = /* redirectURI */window.location.origin + "/";
  var default_001 = /* manifestURI */window.location.origin + "/manifest.json";
  var $$default = /* record */[
    default_000,
    default_001,
    /* appDomain */window.location.origin,
    /* cookieDomain */"misthos.io",
    /* network : Testnet */1
  ];
  var match = window.location.hostname;
  switch (match) {
    case "app.misthos.io" : 
        return /* record */[
                default_000,
                default_001,
                /* appDomain */window.location.origin,
                /* cookieDomain */"misthos.io",
                /* network : Mainnet */2
              ];
    case "localhost" : 
        return /* record */[
                /* redirectURI */"http://localhost:3000/",
                /* manifestURI */"http://localhost:3000/manifest.json",
                /* appDomain */"http://localhost:3000",
                /* cookieDomain */"localhost",
                /* network : Testnet */1
              ];
    case "web-staging.misthos.io" : 
        return /* record */[
                /* redirectURI */"https://staging.misthos.io/",
                /* manifestURI */"https://staging.misthos.io/manifest.json",
                /* appDomain */"https://staging.misthos.io",
                /* cookieDomain */"misthos.io",
                /* network : Testnet */1
              ];
    case "web-testnet.misthos.io" : 
        return /* record */[
                /* redirectURI */"https://testnet.misthos.io/",
                /* manifestURI */"https://testnet.misthos.io/manifest.json",
                /* appDomain */"https://testnet.misthos.io",
                /* cookieDomain */"misthos.io",
                /* network : Testnet */1
              ];
    default:
      return $$default;
  }
}

exports.get = get;
/* No side effect */
