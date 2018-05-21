open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  custodians: list((accountIdx, list(userId))),
  areCurrent: (accountIdx, list(userId)) => bool,
  isCustodian: (accountIdx, userId) => bool,
};

let make = () => {
  custodians: [],
  areCurrent: (_, _) => false,
  isCustodian: (_, _) => false,
};

let update = (event, {custodians}) => {
  let custodians =
    switch (event) {
    | AccountCreationAccepted({data: {accountIdx}}) => [
        (accountIdx, []),
        ...custodians,
      ]
    | CustodianAccepted({data: {accountIdx, partnerId}}) => [
        (accountIdx, [partnerId, ...custodians |> List.assoc(accountIdx)]),
        ...custodians |> List.remove_assoc(accountIdx),
      ]
    | CustodianRemovalAccepted({data: {accountIdx, custodianId}}) => [
        (
          accountIdx,
          custodians
          |> List.assoc(accountIdx)
          |> List.filter(UserId.neq(custodianId)),
        ),
        ...custodians |> List.remove_assoc(accountIdx),
      ]
    | _ => custodians
    };
  {
    custodians,
    areCurrent: (accountIdx, testCustodians) => {
      let accountCustodians = custodians |> List.assoc(accountIdx);
      testCustodians
      |> List.map(c => accountCustodians |> List.mem(c))
      |> List.fold_left((r, v) => r && v, true);
    },
    isCustodian: (accountIdx, testCustodian) =>
      custodians |> List.assoc(accountIdx) |> List.mem(testCustodian),
  };
};
