include ViewCommon;

open Belt;

open PrimitiveTypes;

open WalletTypes;

let component = ReasonReact.statelessComponent("LoggedInHome");

let mandatoryInputs =
  IncomeVenture.viewModel.walletInfoCollector
  |> WalletInfoCollector.nonReservedOldInputs(
       AccountIndex.default,
       UserId.fromString("misthosio.id"),
     );

let defaultFee = BTC.fromSatoshis(100L);

let afterInputs =
  mandatoryInputs
  |. Belt.Set.keep(TransactionFee.canPayForItself(defaultFee));

Js.log2(mandatoryInputs, afterInputs);

let afterInputsCount =
  afterInputs
  |> Set.toArray
  |. Array.reduce(0, (res, {txId}: Network.txInput) =>
       txId
       == "b0478fed46339ffd2d0d36b0355d782be269b0452f452d7532b8f6e1dfa8e06b" ?
         res + 1 : res
     );

let make = (~index, _children) => {
  ...component,
  render: (_) =>
    <div>
      (
        text(
          "there are "
          ++ string_of_int(afterInputsCount)
          ++ " identical things",
        )
      )
    </div>,
};
