type t = {thresholdPercent: float};

let absolute = {thresholdPercent: 100.0};

let fulfilled = (~eligable, ~approved, policy) => {
  let approved = approved |> List.filter(id => eligable |> List.mem(id));
  let nVoters = float(List.length(eligable));
  let nApprovers = float(List.length(approved));
  nApprovers /. nVoters *. 100.0 >= policy.thresholdPercent;
};

let encode = p =>
  Json.Encode.(object_([("thresholdPercent", float(p.thresholdPercent))]));

let decode = raw =>
  Json.Decode.{thresholdPercent: raw |> field("thresholdPercent", float)};
