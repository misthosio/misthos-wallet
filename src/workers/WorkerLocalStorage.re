type t;

[@bs.val] external _localStorage : t = "localStorage";

[@bs.send] [@bs.return nullable]
external _getItem : (t, string) => option(string) = "getItem";

let getItem = key => _getItem(_localStorage, key);

[@bs.send] external _setItem : (t, string, string) => unit = "setItem";

let setItem = (key, value) => _setItem(_localStorage, key, value);

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

let readBlockstackItemsFromStorage = () => {
  blockstack: L.getItem("blockstack", L.localStorage),
  blockstackGaiaHubConfig:
    L.getItem("blockstack-gaia-hub-config", L.localStorage),
  blockstackTransitPrivateKey:
    L.getItem("blockstack-transit-private-key", L.localStorage),
};

let setBlockstackItems =
    ({blockstack, blockstackGaiaHubConfig, blockstackTransitPrivateKey}) => {
  blockstack |> Utils.mapOption(setItem("blockstack")) |> ignore;
  blockstackGaiaHubConfig
  |> Utils.mapOption(setItem("blockstack-gaia-hub-config"))
  |> ignore;
  blockstackTransitPrivateKey
  |> Utils.mapOption(setItem("blockstack-transit-private-key"))
  |> ignore;
};
