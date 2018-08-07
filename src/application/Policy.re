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

module UnanimousMinusOne = {
  let fulfilled = (~eligible: UserId.set, ~endorsed: UserId.set) => {
    let endorsed = Set.intersect(eligible, endorsed);
    endorsed |> Set.size >= Set.size(eligible) - 1 && eligible |> Set.size > 0;
  };
  let canBeFulfilled = (~eligible: UserId.set, ~rejected: UserId.set) => {
    let releventRejections = Set.intersect(eligible, rejected);
    releventRejections |> Set.size <= 1;
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

let canBeFulfilled =
  fun
  | Unanimous => Unanimous.canBeFulfilled
  | UnanimousMinusOne => UnanimousMinusOne.canBeFulfilled;

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

let defaultMetaPolicy = unanimous;
let defaultAddPartner = unanimous;
let defaultAddCustodian = unanimous;
let defaultRemovePartner = unanimousMinusOne;
let defaultRemoveCustodian = unanimousMinusOne;
let defaultPayout = unanimous;
