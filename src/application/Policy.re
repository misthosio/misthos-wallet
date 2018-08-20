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

module Majority = {
  let fulfilled = (~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    let eligibleSize = Set.size(eligible);
    (endorsed |> Set.size)
    * 2 > eligibleSize
    && eligibleSize > 0
    || Unanimous.fulfilled(~eligible, ~endorsed);
  };
  let canBeFulfilled = (~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    (releventRejections |> Set.size) * 2 < Set.size(eligible);
  };
  let encode = _p => Json.Encode.(object_([("type", string("Majority"))]));
};

type t =
  | Unanimous
  | UnanimousMinusOne
  | Majority
  | UnanimousMinusN(UnanimousMinusN.t);

let unanimous = Unanimous;
let unanimousMinusOne = UnanimousMinusOne;
let majority = Majority;
let unanimousMinusN = n => UnanimousMinusN({n: n});

let fulfilled =
  fun
  | Unanimous => Unanimous.fulfilled
  | UnanimousMinusOne => UnanimousMinusOne.fulfilled
  | Majority => Majority.fulfilled
  | UnanimousMinusN(t) => UnanimousMinusN.fulfilled(t);

let canBeFulfilled =
  fun
  | Unanimous => Unanimous.canBeFulfilled
  | UnanimousMinusOne => UnanimousMinusOne.canBeFulfilled
  | Majority => Majority.canBeFulfilled
  | UnanimousMinusN(t) => UnanimousMinusN.canBeFulfilled(t);

let eq = (p1, p2) => p1 == p2;

let neq = (p1, p2) => p1 != p2;

let encode = policy =>
  switch (policy) {
  | Unanimous => Unanimous.encode(policy)
  | UnanimousMinusOne => UnanimousMinusOne.encode(policy)
  | Majority => Majority.encode(policy)
  | UnanimousMinusN(p) => UnanimousMinusN.encode(p)
  };

exception UnknownPolicy(Js.Json.t);

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "Unanimous" => Unanimous
  | "UnanimousMinusOne" => UnanimousMinusOne
  | "Majority" => Majority
  | "UnanimousMinusN" => UnanimousMinusN(UnanimousMinusN.decode(raw))
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
