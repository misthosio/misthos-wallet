module Public = {
  type t = {appPubKey: string};
  let infoFileName = "public.json";
  let encode = data =>
    Json.Encode.(object_([("appPubKey", string(data.appPubKey))]));
  let decode = raw =>
    Json.Decode.{appPubKey: raw |> field("appPubKey", string)};
  let persist = (~appPubKey) =>
    Blockstack.putFile(
      infoFileName,
      encode({appPubKey: appPubKey}) |> Json.stringify,
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
                Private.persist(~chainCode=Crypto.randomBytes(32))
              ),
       )
  );

let storagePrefix = (~appPubKey) =>
  appPubKey
  |> Utils.bufFromHex
  |> Bitcoin.ECPair.fromPublicKeyBuffer
  |> Bitcoin.ECPair.getAddress;
