open Belt;
open PrimitiveTypes;

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
        |> withDefault(Map.String.empty, raw =>
             field("termsAndConditions", dict(string), raw)
             |> Js.Dict.entries
             |> Map.String.fromArray
           ),
    };
  let init = (~appPubKey) => {
    let res = {appPubKey, termsAndConditions: Map.String.empty};
    Blockstack.putFileNotEncrypted(
      infoFileName,
      encode(res) |> Json.stringify,
    )
    |> Js.Promise.(then_(_ => res |> resolve));
  };
  let persist = info =>
    Blockstack.putFileNotEncrypted(
      infoFileName,
      encode(info) |> Json.stringify,
    )
    |> Js.Promise.(then_(_ => info |> resolve));
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

let hasSignedTAC = (tacHash, userInfo: Public.t) =>
  switch (userInfo.termsAndConditions |. Map.String.get(tacHash)) {
  | Some(signature) =>
    let signature =
      Bitcoin.ECSignature.fromDER(signature |> Utils.bufFromHex);
    Utils.keyFromPublicKey(userInfo.appPubKey)
    |> Bitcoin.ECPair.verify(tacHash |> Utils.bufFromHex, signature);
  | _ => false
  };

let signTAC = (tacHash, privateKey, network, userInfo: Public.t) => {
  let keyPair =
    Utils.keyPairFromPrivateKey(
      network |> Network.bitcoinNetwork,
      privateKey,
    );
  let signature =
    keyPair
    |> Bitcoin.ECPair.sign(tacHash |> Utils.bufFromHex)
    |> Bitcoin.ECSignature.toDER
    |> Utils.bufToHex;
  {
    ...userInfo,
    termsAndConditions:
      userInfo.termsAndConditions |. Map.String.set(tacHash, signature),
  }
  |> Public.persist;
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

let getOrInit = (~appPubKey, userId) =>
  Js.Promise.(
    all2((
      Private.read(),
      Public.read(~blockstackId=userId |> UserId.toString),
    ))
    |> then_(
         fun
         | (Private.Ok(info), Public.Ok(public)) =>
           (info, public) |> resolve
         | _ =>
           Public.init(~appPubKey)
           |> then_(pub_ =>
                Private.persist(
                  ~chainCode=
                    appPubKey |. String.sub(0, 64) |> Utils.bufFromHex,
                )
                |> then_(priv => (priv, pub_) |> resolve)
              ),
       )
  );

let storagePrefix = (~appPubKey) =>
  appPubKey
  |> Utils.bufFromHex
  |> Bitcoin.ECPair.fromPublicKeyBuffer
  |> Bitcoin.ECPair.getAddress;
