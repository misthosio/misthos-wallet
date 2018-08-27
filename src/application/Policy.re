open Belt;

open PrimitiveTypes;

module Unanimous = {
  let fulfilled = (~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    endorsed |> Set.size >= Set.size(eligible) && eligible |> Set.size > 0;
  };
  let canBeFulfilled = (~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    releventRejections |> Set.size == 0;
  };
  let encode = _p => Json.Encode.(object_([("type", string("Unanimous"))]));
};

module UnanimousMinusN = {
  type t = {n: int};
  let fulfilled = ({n}, ~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    let nEndorsed = endorsed |> Set.size;
    nEndorsed >= 1
    && nEndorsed >= Set.size(eligible)
    - n
    && eligible
    |> Set.size > 0;
  };
  let canBeFulfilled = ({n}, ~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    releventRejections |> Set.size <= n;
  };
  let encode = ({n}) =>
    Json.Encode.(
      object_([("type", string("UnanimousMinusN")), ("n", int(n))])
    );
  let decode = raw => Json.Decode.{n: raw |> field("n", int)};
};

module UnanimousMinusOne = {
  let fulfilled = UnanimousMinusN.fulfilled({n: 1});
  let canBeFulfilled = UnanimousMinusN.canBeFulfilled({n: 1});
  let encode = _p =>
    Json.Encode.(object_([("type", string("UnanimousMinusOne"))]));
};

module AtLeast = {
  type t = {n: int};
  let fulfilled = ({n}, ~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    let nEndorsed = endorsed |> Set.size;
    nEndorsed >= 1
    && nEndorsed >= n
    && eligible
    |> Set.size > 0
    || Unanimous.fulfilled(~eligible, ~endorsed);
  };
  let canBeFulfilled = ({n}, ~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    Set.size(eligible) - Set.size(releventRejections) >= n;
  };
  let encode = ({n}) =>
    Json.Encode.(object_([("type", string("AtLeast")), ("n", int(n))]));
  let decode = raw => Json.Decode.{n: raw |> field("n", int)};
};

module Percentage = {
  type t = {percentage: int};
  let fulfilled =
      ({percentage}, ~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    let eligibleSize = Set.size(eligible);
    eligibleSize > 0
    && endorsed
    |> Set.size
    |> float_of_int > float_of_int(eligibleSize)
    *. (float_of_int(percentage) /. 100.)
    || Unanimous.fulfilled(~eligible, ~endorsed);
  };
  let canBeFulfilled =
      ({percentage}, ~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    releventRejections
    |> Set.size
    |> float_of_int < float_of_int(Set.size(eligible))
    *. (float_of_int(100 - percentage) /. 100.);
  };
  let encode = ({percentage}) =>
    Json.Encode.(
      object_([
        ("type", string("Percentage")),
        ("percentage", int(percentage)),
      ])
    );
  let decode = raw =>
    Json.Decode.{percentage: raw |> field("percentage", int)};
};

type t =
  | Unanimous
  | UnanimousMinusOne
  | Percentage(Percentage.t)
  | AtLeast(AtLeast.t);

let unanimous = Unanimous;
let unanimousMinusOne = UnanimousMinusOne;
let percentage = percentage => Percentage({percentage: percentage});
let atLeast = n => AtLeast({n: n});

let fulfilled =
  fun
  | Unanimous => Unanimous.fulfilled
  | UnanimousMinusOne => UnanimousMinusOne.fulfilled
  | Percentage(t) => Percentage.fulfilled(t)
  | AtLeast(t) => AtLeast.fulfilled(t);

let canBeFulfilled =
  fun
  | Unanimous => Unanimous.canBeFulfilled
  | UnanimousMinusOne => UnanimousMinusOne.canBeFulfilled
  | Percentage(t) => Percentage.canBeFulfilled(t)
  | AtLeast(t) => AtLeast.canBeFulfilled(t);

let eq = (p1, p2) => p1 == p2;

let neq = (p1, p2) => p1 != p2;

let encode = policy =>
  switch (policy) {
  | Unanimous => Unanimous.encode(policy)
  | UnanimousMinusOne => UnanimousMinusOne.encode(policy)
  | Percentage(p) => Percentage.encode(p)
  | AtLeast(p) => AtLeast.encode(p)
  };

exception UnknownPolicy(Js.Json.t);

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "Unanimous" => Unanimous
  | "UnanimousMinusOne" => UnanimousMinusOne
  | "Percentage" => Percentage(Percentage.decode(raw))
  | "AtLeast" => AtLeast(AtLeast.decode(raw))
  | _ => raise(UnknownPolicy(raw))
  };
};

let defaultMetaPolicy = unanimous;
let defaultAddPartner = unanimous;
let defaultAddCustodian = unanimous;
let defaultRemovePartner = unanimousMinusOne;
let defaultRemoveCustodian = unanimousMinusOne;
let defaultPayout = unanimous;

type initialPolicies = {
  addPartner: t,
  addCustodian: t,
  removePartner: t,
  removeCustodian: t,
  payout: t,
};
let defaultInitialPolicies = {
  addPartner: defaultAddPartner,
  addCustodian: defaultAddCustodian,
  removePartner: defaultRemovePartner,
  removeCustodian: defaultRemoveCustodian,
  payout: defaultPayout,
};
let encodeInitialPolicies = policies =>
  Json.Encode.(
    object_([
      ("addPartner", encode(policies.addPartner)),
      ("addCustodian", encode(policies.addCustodian)),
      ("removePartner", encode(policies.removePartner)),
      ("removeCustodian", encode(policies.removeCustodian)),
      ("payout", encode(policies.payout)),
    ])
  );
let decodeInitialPolicies = raw =>
  Json.Decode.{
    addPartner: raw |> field("addPartner", decode),
    addCustodian: raw |> field("addCustodian", decode),
    removePartner: raw |> field("removePartner", decode),
    removeCustodian: raw |> field("removeCustodian", decode),
    payout: raw |> field("payout", decode),
  };
