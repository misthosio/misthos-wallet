open Belt;

let rec pruneNullKeys = (json: Js.Json.t) =>
  if (Js.typeof(json) == "object"
      && ! Js.Array.isArray(json)
      && ! (Obj.magic(json) == Js.null)) {
    json
    |> Obj.magic
    |> Js.Dict.entries
    |. Array.keepMapU((. (key, v)) =>
         Obj.magic(v) == Js.null ? None : Some((key, v))
       )
    |> Js.Dict.fromArray
    |> Obj.magic;
  } else if (Js.Array.isArray(json)) {
    json |> Obj.magic |. Array.map(pruneNullKeys) |> Obj.magic;
  } else {
    json;
  };
