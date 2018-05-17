open PrimitiveTypes;

let filterUsers = (~eligible, ~endorsed) =>
  endorsed |> List.filter(user => eligible |> List.mem(user));

module Unanimous = {
  let fulfilled = (~eligible: list(userId), ~endorsed: list(userId)) => {
    let endorsed = filterUsers(~eligible, ~endorsed);
    eligible
    |> List.length == (endorsed |> List.length)
    && eligible
    |> List.length > 0;
  };
  let encode = _p => Json.Encode.(object_([("type", string("Unanimous"))]));
};

module UnanimousMinusOne = {
  let fulfilled = (~eligible: list(userId), ~endorsed: list(userId)) => {
    let endorsed = filterUsers(~eligible, ~endorsed);
    endorsed
    |> List.length >= List.length(eligible)
    - 1
    && eligible
    |> List.length > 0;
  };
  let encode = _p =>
    Json.Encode.(object_([("type", string("UnanimousMinusOne"))]));
};

type t =
  | Unanimous
  | UnanimousMinusOne;

let unanimous = Unanimous;

let unanimousMinusOne = UnanimousMinusOne;

let fulfilled =
  fun
  | Unanimous => Unanimous.fulfilled
  | UnanimousMinusOne => UnanimousMinusOne.fulfilled;

let eq = (p1, p2) => p1 == p2;

let neq = (p1, p2) => p1 != p2;

let encode = policy =>
  switch (policy) {
  | Unanimous => Unanimous.encode(policy)
  | UnanimousMinusOne => UnanimousMinusOne.encode(policy)
  };

exception UnknownPolicy(Js.Json.t);

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "Unanimous" => Unanimous
  | "UnanimousMinusOne" => UnanimousMinusOne
  | _ => raise(UnknownPolicy(raw))
  };
};
