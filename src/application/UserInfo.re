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
      encode({appPubKey: appPubKey}) |> Json.stringify
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
