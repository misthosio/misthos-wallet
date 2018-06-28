open PrimitiveTypes;

type item = {
  id: ventureId,
  name: string,
};

type t = {
  ventures: list(item),
  breakingChange: bool,
};
let makeIndex = breakingChange => {ventures: [], breakingChange};

module Encode = {
  let item = item =>
    Json.Encode.(
      object_([
        ("name", string(item.name)),
        ("id", VentureId.encode(item.id)),
      ])
    );
  let ventures = Json.Encode.list(item);

  let index = index =>
    Json.Encode.(
      object_([
        ("ventures", ventures(index.ventures)),
        ("breakingChange", bool(index.breakingChange)),
      ])
    );
};

module Decode = {
  let item = json =>
    Json.Decode.{
      name: json |> field("name", string),
      id: json |> field("id", VentureId.decode),
    };
  let ventures = Json.Decode.list(item);
  let index = json =>
    Json.Decode.{
      ventures: json |> field("ventures", ventures),
      breakingChange: json |> field("breakingChange", bool),
    };
};

let encode = Encode.index;

let decode = Decode.index;

let indexPath = "index.json";

let persist = index =>
  Js.Promise.(
    Blockstack.putFileEncrypted(
      indexPath,
      Encode.index(index) |> Json.stringify,
    )
    |> then_(() => resolve(index))
  );

let persistOld = index =>
  Blockstack.putFileEncrypted(
    "indexV0.json",
    Encode.ventures(index) |> Json.stringify,
  );

let load = () =>
  Js.Promise.(
    Blockstack.getFileDecrypted(indexPath)
    |> then_(nullVentures =>
         switch (Js.Nullable.toOption(nullVentures)) {
         | None => persist(makeIndex(false))
         | Some(index) =>
           let parsedIndex = index |> Json.parseOrRaise;
           try (parsedIndex |> Decode.index |> resolve) {
           | _ =>
             try (
               {
                 let oldIndex = Decode.ventures(parsedIndex);
                 if (oldIndex |> List.length > 0) {
                   persistOld(oldIndex) |> ignore;
                   makeIndex(true) |> persist;
                 } else {
                   makeIndex(false) |> persist;
                 };
               }
             ) {
             | _ => makeIndex(false) |> persist
             }
           };
         }
       )
  );

let itemPresent = (ventureId, {ventures}) =>
  ventures |> List.exists(item => VentureId.eq(item.id, ventureId));

let add = (~ventureId as id, ~ventureName as name) =>
  Js.Promise.(
    load()
    |> then_(index =>
         if (index |> itemPresent(id)) {
           index |> resolve;
         } else {
           {
             breakingChange: false,
             ventures: [{id, name}, ...index.ventures],
           }
           |> persist;
         }
       )
  );
