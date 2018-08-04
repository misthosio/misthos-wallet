let defaultCoSignerList = [|0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8|];
let defaultSequence = 6 * 24 * 88;

type t = {
  coSignerList: array(int),
  sequence: option(int),
};

let defaultSettings = {
  coSignerList: defaultCoSignerList,
  sequence: Some(defaultSequence),
};

let encode = settings =>
  Json.Encode.(
    object_([
      ("coSignerList", array(int, settings.coSignerList)),
      ("sequence", nullable(int, settings.sequence)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    coSignerList: raw |> field("coSignerList", array(int)),
    sequence: raw |> optional(int),
  };
