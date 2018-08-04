open PrimitiveTypes;

open WalletTypes;

module Identifier = {
  type t = string;
  let encode = Json.Encode.string;
  let decode = Json.Decode.string;
  let make = (nCoSigners, custodianKeyChains) =>
    custodianKeyChains
    |> List.sort(((userId1, _), (userId2, _)) =>
         UserId.compare(userId1, userId2)
       )
    |> List.map(((userId, keyChain)) =>
         (
           userId,
           keyChain |> CustodianKeyChain.hdNode |> Bitcoin.HDNode.toBase58,
         )
       )
    |> List.fold_left(
         (res, (userId, xpub)) => res ++ UserId.toString(userId) ++ xpub,
         nCoSigners |> string_of_int,
       )
    |> Utils.hash;
  let neq = (a, b) => a != b;
  let eq = (a, b) => a == b;
};

type t = {
  accountIdx,
  identifier: Identifier.t,
  nCoSigners: int,
  sequence: option(int),
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
};

let defaultCoSignerList = [|0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8|];
let defaultSequence = 6 * 24 * 88;

let make =
    (
      ~settings=AccountSettings.defaultSettings,
      accountIdx,
      custodianKeyChains,
    ) => {
  let nCoSigners = settings.coSignerList[custodianKeyChains |> List.length];
  {
    accountIdx,
    identifier: Identifier.make(nCoSigners, custodianKeyChains),
    custodianKeyChains,
    nCoSigners,
    sequence: nCoSigners > 1 ? settings.sequence : None,
  };
};

let isConsistent = ({custodianKeyChains, identifier, nCoSigners}) =>
  nCoSigners == defaultCoSignerList[custodianKeyChains |> List.length]
  && identifier
  |> Identifier.eq(Identifier.make(nCoSigners, custodianKeyChains));

let custodians = ({custodianKeyChains}) =>
  custodianKeyChains
  |> List.map(fst)
  |> Array.of_list
  |> Belt.Set.mergeMany(UserId.emptySet);

module Collection = {
  open Belt;
  type nonrec t = Map.String.t(t);
  let empty = Map.String.empty;
  let add = ({identifier} as keyChain, collection) =>
    collection |. Map.String.set(identifier, keyChain);
  let lookup = (_accountIdx, identifier, keyChains: t) =>
    keyChains |. Map.String.getExn(identifier);
};

let encode = keyChain =>
  Json.Encode.(
    object_([
      (
        "custodianKeyChains",
        list(
          pair(UserId.encode, CustodianKeyChain.encode),
          keyChain.custodianKeyChains,
        ),
      ),
      ("nCoSigners", int(keyChain.nCoSigners)),
      ("accountIdx", AccountIndex.encode(keyChain.accountIdx)),
      ("identifier", Identifier.encode(keyChain.identifier)),
      ("sequence", nullable(int, keyChain.sequence)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    custodianKeyChains:
      raw
      |> field(
           "custodianKeyChains",
           list(pair(UserId.decode, CustodianKeyChain.decode)),
         ),
    nCoSigners: raw |> field("nCoSigners", int),
    accountIdx: raw |> field("accountIdx", AccountIndex.decode),
    identifier: raw |> field("identifier", Identifier.decode),
    sequence: raw |> optional(field("sequence", int)),
  };
