open Belt;

module Public = {
  type t = {
    appPubKey: string,
    termsAndConditions: Map.String.t(string),
  };
  let infoFileName = "public.json";
  let encode = data =>
    Json.Encode.(
      object_([
        ("appPubKey", string(data.appPubKey)),
        (
          "termsAndConditions",
          data.termsAndConditions
          |. Map.String.toArray
          |. Array.mapU((. (k, v)) => (k, v |> string))
          |> Js.Dict.fromArray
          |> dict,
        ),
      ])
    );
  let decode = raw =>
    Json.Decode.{
      appPubKey: raw |> field("appPubKey", string),
      termsAndConditions:
        raw
        |> field("termsAndConditions", dict(string))
        |> Js.Dict.entries
        |> Map.String.fromArray,
    };
  let persist = (~appPubKey) =>
    Blockstack.putFileNotEncrypted(
      infoFileName,
      encode({appPubKey, termsAndConditions: Map.String.empty})
      |> Json.stringify,
    );
  type readResult =
    | NotFound
    | Ok(t);
  let read = (~blockstackId as username) =>
    Js.Promise.(
      Blockstack.getFileFromUser(infoFileName, ~username)
      |> then_(nullFile =>
           switch (Js.Nullable.toOption(nullFile)) {
           | None => resolve(NotFound)
           | Some(raw) => resolve(Ok(raw |> Json.parseOrRaise |> decode))
           }
         )
      |> catch(error => {
           Utils.printError("Couldn't fetch public.json", error);
           resolve(NotFound);
         })
    );
};

module Private = {
  type t = {chainCode: Node.buffer};
  let infoFileName = "private.json";
  let encode = data =>
    Json.Encode.(
      object_([("chainCode", string(data.chainCode |> Utils.bufToHex))])
    );
  let decode = raw =>
    Json.Decode.{
      chainCode: raw |> field("chainCode", string) |> Utils.bufFromHex,
    };
  let persist = (~chainCode) =>
    Js.Promise.(
      Blockstack.putFileEncrypted(
        infoFileName,
        encode({chainCode: chainCode}) |> Json.stringify,
      )
      |> then_(_result => resolve({chainCode: chainCode}))
    );
  type readResult =
    | NotFound
    | Ok(t);
  let read = () =>
    Js.Promise.(
      Blockstack.getFileDecrypted(infoFileName)
      |> then_(nullFile =>
           switch (Js.Nullable.toOption(nullFile)) {
           | None => resolve(NotFound)
           | Some(raw) => resolve(Ok(raw |> Json.parseOrRaise |> decode))
           }
         )
      |> catch(error => {
           Utils.printError("Couldn't fetch private.json", error);
           resolve(NotFound);
         })
    );
};

let getOrInit = (~appPubKey) =>
  Js.Promise.(
    Private.read()
    |> then_(
         fun
         | Private.Ok(info) => info |> resolve
         | Private.NotFound =>
           Public.persist(~appPubKey)
           |> then_(_result =>
                Private.persist(
                  ~chainCode=
                    appPubKey |. String.sub(0, 64) |> Utils.bufFromHex,
                )
              ),
       )
  );

let storagePrefix = (~appPubKey) =>
  appPubKey
  |> Utils.bufFromHex
  |> Bitcoin.ECPair.fromPublicKeyBuffer
  |> Bitcoin.ECPair.getAddress;
