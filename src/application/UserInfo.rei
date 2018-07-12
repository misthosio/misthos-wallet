open Belt;
open PrimitiveTypes;

module Public: {
  type t = {
    appPubKey: string,
    termsAndConditions: Map.String.t(string),
  };
  type readResult =
    | NotFound
    | Ok(t);
  let read: (~blockstackId: userId) => Js.Promise.t(readResult);
};
module Private: {type t = {chainCode: Node.buffer};};

let storagePrefix: (~appPubKey: string) => string;
let getOrInit: (~appPubKey: string) => Js.Promise.t((Private.t, Public.t));
