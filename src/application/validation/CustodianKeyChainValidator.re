open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  keyChains:
    list((accountIdx, list((userId, list(CustodianKeyChain.public))))),
  allExist: (accountIdx, list((userId, CustodianKeyChain.public))) => bool,
};

let make = () => {keyChains: [], allExist: (_, _) => false};

let update = (event, {keyChains}) => {
  let keyChains =
    switch (event) {
    | AccountCreationAccepted({data: {accountIdx}}) => [
        (accountIdx, []),
        ...keyChains,
      ]
    | CustodianKeyChainUpdated({custodianId, keyChain}) =>
      let accountIdx = keyChain |> CustodianKeyChain.accountIdx;
      let accountChains = keyChains |> List.assoc(accountIdx);
      let userChains =
        try (accountChains |> List.assoc(custodianId)) {
        | Not_found => []
        };
      [
        (
          keyChain |> CustodianKeyChain.accountIdx,
          [
            (custodianId, [keyChain, ...userChains]),
            ...accountChains |> List.remove_assoc(custodianId),
          ],
        ),
        ...keyChains |> List.remove_assoc(accountIdx),
      ];
    | _ => keyChains
    };
  {
    keyChains,
    allExist: (accountIdx, testChains) =>
      try (
        {
          let accountChains = keyChains |> List.assoc(accountIdx);
          testChains
          |> List.map(((user, chain)) =>
               accountChains
               |> List.assoc(user)
               |> List.exists(CustodianKeyChain.eq(chain))
             )
          |> List.fold_left((r, v) => r && v, true);
        }
      ) {
      | Not_found => false
      },
  };
};
