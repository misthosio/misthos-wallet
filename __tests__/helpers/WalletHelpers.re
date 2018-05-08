open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

module Wallet = Venture__Wallet;

module G = Generators;

module L = G.Log;

let constructState = log =>
  log |> L.reduce((s, {event}) => s |> Wallet.apply(event), Wallet.make());

let testNextIncomeAddress = (user: Session.Data.t, address, wallet) => {
  let exposed =
    wallet
    |> Wallet.exposeNextIncomeAddress(user.userId, AccountIndex.default);
  let description =
    "the next address of '"
    ++ UserId.toString(user.userId)
    ++ "' is '"
    ++ address
    ++ "'";
  test(description, () =>
    expect(exposed.address) |> toEqual(address)
  );
  wallet |> Wallet.apply(IncomeAddressExposed(exposed));
};
