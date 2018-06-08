open LocalStorage;

type blockstackItems = {
  blockstack: option(string),
  blockstackGaiaHubConfig: option(string),
  blockstackTransitPrivateKey: option(string),
};

let encodeItems = items =>
  Json.Encode.(
    object_([
      ("blockstack", nullable(string, items.blockstack)),
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
    blockstack: raw |> field("blockstack", optional(string)),
    blockstackGaiaHubConfig:
      raw |> field("blockstackGaiaHubConfig", optional(string)),
    blockstackTransitPrivateKey:
      raw |> field("blockstackTransitPrivateKey", optional(string)),
  };

module L = Dom.Storage;

let blockstackKey = "blockstack";
let gaiaHubKey = "blockstack-gaia-hub-config";
let transitPrivKey = "blockstack-transit-private-key";

let readBlockstackItemsFromStorage = () => {
  blockstack: L.getItem(blockstackKey, L.localStorage),
  blockstackGaiaHubConfig: L.getItem(gaiaHubKey, L.localStorage),
  blockstackTransitPrivateKey: L.getItem(transitPrivKey, L.localStorage),
};

let setBlockstackItems =
    ({blockstack, blockstackGaiaHubConfig, blockstackTransitPrivateKey}) => {
  switch (blockstack) {
  | Some(blockstack) => blockstack |> setItem(blockstackKey)
  | None => removeItem(blockstackKey)
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
