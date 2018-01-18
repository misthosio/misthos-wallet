type item = {
  id: string,
  name: string
};

type t = list(item);

module Encode = {
  let item = item =>
    Json.Encode.(
      object_([("name", string(item.name)), ("id", string(item.id))])
    );
  let index = Json.Encode.list(item);
};

module Decode = {
  let item = json =>
    Json.Decode.{
      name: json |> field("name", string),
      id: json |> field("id", string)
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
    |> then_(nullDeals =>
         switch (Js.Nullable.to_opt(nullDeals)) {
         | None => persist([])
         | Some(index) => resolve(index |> Json.parseOrRaise |> Decode.index)
         }
       )
  );

let add = (~dealId as id, ~dealName as name) =>
  Js.Promise.(load() |> then_(index => [{id, name}, ...index] |> persist));
