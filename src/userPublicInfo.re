type t = {appPubKey: string};

let infoFileName = "public.json";

let encode = data =>
  Json.Encode.(object_([("appPubKey", string(data.appPubKey))]));

let decode = raw => Json.Decode.{appPubKey: raw |> field("appPubKey", string)};

let persist = (~appPubKey) =>
  Blockstack.putFile(
    infoFileName,
    encode({appPubKey: appPubKey}) |> Json.stringify
  );

type readResult =
  | NotFound
  | Ok(t);

let read = (~blockstackId as username) =>
  Js.Promise.(
    Blockstack.getFileWithOpts(infoFileName, ~username, ())
    |> then_(nullFile =>
         switch (Js.Nullable.to_opt(nullFile)) {
         | None => resolve(NotFound)
         | Some(raw) => resolve(Ok(raw |> Json.parseOrRaise |> decode))
         }
       )
    |> catch(_error => resolve(NotFound))
  );
