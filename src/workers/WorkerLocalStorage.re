open LocalStorage;

type blockstackItems = {
  blockstackSession: option(string),
  blockstackGaiaHubConfig: option(string),
  blockstackTransitPrivateKey: option(string),
};

let encodeItems = items =>
  Json.Encode.(
    object_([
      ("blockstackSession", nullable(string, items.blockstackSession)),
      (
        "blockstackGaiaHubConfig",
        nullable(string, items.blockstackGaiaHubConfig),
      ),
      (
        "blockstackTransitPrivateKey",
        nullable(string, items.blockstackTransitPrivateKey),
      ),
    ])
  );

let decodeItems = raw =>
  Json.Decode.{
    blockstackSession: raw |> field("blockstackSession", optional(string)),
    blockstackGaiaHubConfig:
      raw |> field("blockstackGaiaHubConfig", optional(string)),
    blockstackTransitPrivateKey:
      raw |> field("blockstackTransitPrivateKey", optional(string)),
  };

module L = Dom.Storage;

let blockstackSessionKey = "blockstack-session";
let gaiaHubKey = "blockstack-gaia-hub-config";
let transitPrivKey = "blockstack-transit-private-key";

let readBlockstackItemsFromStorage = () => {
  blockstackSession: L.getItem(blockstackSessionKey, L.localStorage),
  blockstackGaiaHubConfig: L.getItem(gaiaHubKey, L.localStorage),
  blockstackTransitPrivateKey: L.getItem(transitPrivKey, L.localStorage),
};

let setBlockstackItems =
    ({blockstackSession, blockstackGaiaHubConfig, blockstackTransitPrivateKey}) => {
  switch (blockstackSession) {
  | Some(blockstackSession) => blockstackSession |> setItem(blockstackSessionKey)
  | None => removeItem(blockstackSessionKey)
  };
  switch (blockstackGaiaHubConfig) {
  | Some(config) => config |> setItem(gaiaHubKey)
  | None => removeItem(gaiaHubKey)
  };
  switch (blockstackTransitPrivateKey) {
  | Some(key) => key |> setItem(transitPrivKey)
  | None => removeItem(transitPrivKey)
  };
};
