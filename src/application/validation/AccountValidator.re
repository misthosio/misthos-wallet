open WalletTypes;

open Event;

type t = {
  accounts: list(accountIdx),
  exists: accountIdx => bool,
};

let make = () => {accounts: [], exists: _ => false};

let update = (event, {accounts}) => {
  let accounts =
    switch (event) {
    | AccountCreationAccepted({data: {accountIdx}}) => [
        accountIdx,
        ...accounts,
      ]
    | _ => accounts
    };
  {accounts, exists: accountIdx => accounts |> List.mem(accountIdx)};
};
