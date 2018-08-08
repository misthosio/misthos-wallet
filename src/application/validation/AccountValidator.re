open Belt;

open WalletTypes;

open Event;

type t = {
  accounts: AccountIndex.map(AccountSettings.t),
  settings: accountIdx => option(AccountSettings.t),
};

let make = () => {accounts: AccountIndex.makeMap(), settings: _ => None};

let update = (event, {accounts}) => {
  let accounts =
    switch (event) {
    | AccountCreationAccepted({data: {accountIdx, settings}}) =>
      accounts
      |. Map.set(
           accountIdx,
           settings |> Js.Option.getWithDefault(AccountSettings.default),
         )
    | _ => accounts
    };
  {accounts, settings: accountIdx => accounts |. Map.get(accountIdx)};
};
