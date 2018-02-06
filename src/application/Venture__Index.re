open PrimitiveTypes;

type item = {
  id: ventureId,
  name: string
};

type t = list(item);

module Encode = {
  let item = item =>
    Json.Encode.(
      object_([("name", string(item.name)), ("id", VentureId.encode(item.id))])
    );
  let index = Json.Encode.list(item);
};

module Decode = {
  let item = json =>
    Json.Decode.{
      name: json |> field("name", string),
      id: json |> field("id", VentureId.decode)
    };
  let index = Json.Decode.list(item);
};

let indexPath = "index.json";

let persist = index =>
  Js.Promise.(
    Blockstack.putFileEncrypted(
      indexPath,
      Encode.index(index) |> Json.stringify
    )
    |> then_(() => resolve(index))
  );

let load = () =>
  Js.Promise.(
    Blockstack.getFileDecrypted(indexPath)
    |> then_(nullVentures =>
         switch (Js.Nullable.to_opt(nullVentures)) {
         | None => persist([])
         | Some(index) => resolve(index |> Json.parseOrRaise |> Decode.index)
         }
       )
  );

let add = (~ventureId as id, ~ventureName as name) =>
  Js.Promise.(load() |> then_(index => [{id, name}, ...index] |> persist));
