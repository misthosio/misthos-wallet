open PrimitiveTypes;

type t = {thresholdPercent: float};

let absolute = {thresholdPercent: 100.0};

let fulfilled = (~eligable: list(userId), ~endorsed: list(userId), policy) => {
  let endorsed = endorsed |> List.filter(id => eligable |> List.mem(id));
  let nVoters = float(List.length(eligable));
  let nEndorsers = float(List.length(endorsed));
  nEndorsers /. nVoters *. 100.0 >= policy.thresholdPercent;
};

let eq = (p1, p2) => p1.thresholdPercent == p2.thresholdPercent;

let neq = (p1, p2) => p1.thresholdPercent != p2.thresholdPercent;

let encode = p =>
  Json.Encode.(
    object_([("thresholdPercent", Json.Encode.float(p.thresholdPercent))])
  );

let decode = raw =>
  Json.Decode.{
    thresholdPercent: raw |> field("thresholdPercent", Json.Decode.float),
  };
