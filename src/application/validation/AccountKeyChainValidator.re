open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  identified: list((accountIdx, list(AccountKeyChain.Identifier.t))),
  activations:
    list((userId, list((AccountKeyChain.Identifier.t, list(int))))),
  exists: (accountIdx, AccountKeyChain.Identifier.t) => bool,
  inOrder: (userId, AccountKeyChain.Identifier.t, int) => bool,
};

let make = () => {
  identified: [],
  activations: [],
  exists: (_, _) => false,
  inOrder: (_, _, _) => false,
};

let getOrEmpty = (item, theList) =>
  try (theList |> List.assoc(item)) {
  | Not_found => []
  };

let update = (event, {identified, activations}) => {
  let (identified, activations) =
    switch (event) {
    | AccountCreationAccepted({data: {accountIdx}}) => (
        [(accountIdx, []), ...identified],
        activations,
      )
    | AccountKeyChainIdentified({keyChain: {accountIdx, identifier}}) => (
        [
          (
            accountIdx,
            [identifier, ...identified |> List.assoc(accountIdx)],
          ),
          ...identified |> List.remove_assoc(accountIdx),
        ],
        activations,
      )
    | AccountKeyChainActivated({custodianId, identifier, sequence}) => (
        identified,
        [
          (
            custodianId,
            [
              (
                identifier,
                [
                  sequence,
                  ...activations
                     |> getOrEmpty(custodianId)
                     |> getOrEmpty(identifier),
                ],
              ),
            ],
          ),
        ],
      )
    | _ => (identified, activations)
    };
  {
    identified,
    activations,
    exists: (accountIdx, identifier) =>
      identified |> List.assoc(accountIdx) |> List.mem(identifier),
    inOrder: (custodianId, identifier, sequence) =>
      activations
      |> getOrEmpty(custodianId)
      |> getOrEmpty(identifier)
      |> List.length == sequence,
  };
};
