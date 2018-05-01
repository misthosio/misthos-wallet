open PrimitiveTypes;

let filterUsers = (~eligable, ~endorsed) =>
  endorsed |> List.filter(user => eligable |> List.mem(user));

module Unanimous = {
  let fulfilled = (~eligable: list(userId), ~endorsed: list(userId)) => {
    let endorsed = filterUsers(~eligable, ~endorsed);
    eligable
    |> List.length == (endorsed |> List.length)
    && eligable
    |> List.length > 0;
  };
  let encode = _p => Json.Encode.(object_([("type", string("Unanimous"))]));
};

module UnanimousMinusOne = {
  let fulfilled = (~eligable: list(userId), ~endorsed: list(userId)) => {
    let endorsed = filterUsers(~eligable, ~endorsed);
    endorsed
    |> List.length >= List.length(eligable)
    - 1
    && eligable
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
