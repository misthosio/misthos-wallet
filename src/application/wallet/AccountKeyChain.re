open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

type t = {
  accountIdx,
  keyChainIdx: accountKeyChainIdx,
  nCoSigners: int,
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
};

let defaultCoSignerList = [|0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8|];

let make = (accountIdx, keyChainIdx, custodianKeyChains) => {
  accountIdx,
  keyChainIdx,
  custodianKeyChains,
  nCoSigners: defaultCoSignerList[custodianKeyChains |> List.length],
};

module Collection = {
  type collection = list((accountIdx, list((accountKeyChainIdx, t))));
  type t = collection;
  let make = () => [];
  let add = ({accountIdx, keyChainIdx} as keyChain, collection) => {
    let keyChains =
      try (collection |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    [
      (accountIdx, [(keyChainIdx, keyChain), ...keyChains]),
      ...collection |> List.remove_assoc(accountIdx),
    ];
  };
  let lookup = (accountIdx, accountKeyChainIdx, accounts: t) =>
    accounts |> List.assoc(accountIdx) |> List.assoc(accountKeyChainIdx);
  let latest = (accountIdx, accounts: t) =>
    accounts
    |> List.assoc(accountIdx)
    |> List.fold_left(
         (res, (keyChainIdx, keyChain)) =>
           AccountKeyChainIndex.compare(keyChainIdx, res.keyChainIdx) > 0 ?
             keyChain : res,
         accounts |> List.assoc(accountIdx) |> List.hd |> snd,
       );
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
      ("keyChainIdx", AccountKeyChainIndex.encode(keyChain.keyChainIdx)),
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
    keyChainIdx: raw |> field("keyChainIdx", AccountKeyChainIndex.decode),
  };
