type t;

[@bs.val] external _localStorage : t = "localStorage";

[@bs.send] [@bs.return nullable]
external _getItem : (t, string) => option(string) = "getItem";

let getItem = key => _getItem(_localStorage, key);

[@bs.send] external _setItem : (t, string, string) => unit = "setItem";

let setItem = (key, value) => _setItem(_localStorage, key, value);

[@bs.send] external _removeItem : (t, string) => unit = "removeItem";

let removeItem = key => _removeItem(_localStorage, key);

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
