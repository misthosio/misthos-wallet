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

let collectNextTwoAddresses = (user: Session.Data.t, (wallet, log)) => {
  let exposed =
    wallet
    |> Wallet.exposeNextIncomeAddress(user.userId, AccountIndex.default);
  let address1 = exposed.address;
  let log = log |> L.appendSystemEvent(IncomeAddressExposed(exposed));
  let wallet = wallet |> Wallet.apply(IncomeAddressExposed(exposed));
  let exposed =
    wallet
    |> Wallet.exposeNextIncomeAddress(user.userId, AccountIndex.default);
  let address2 = exposed.address;
  let log = log |> L.appendSystemEvent(IncomeAddressExposed(exposed));
  let wallet = wallet |> Wallet.apply(IncomeAddressExposed(exposed));
  ((address1, address2), (wallet, log));
};

let getExposedAddresses = ({exposedCoordinates, accountKeyChains}: Wallet.t) =>
  exposedCoordinates
  |> List.map(coordinates => accountKeyChains |> Address.find(coordinates))
  |> List.map((a: Address.t) => a.address);
