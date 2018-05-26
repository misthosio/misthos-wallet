open Belt;

open PrimitiveTypes;

open WalletTypes;

let text = ReasonReact.string;

let component = ReasonReact.statelessComponent("LoggedInHome");

let reproWalletCollector =
  IncomeVenture.eventLog
  |> EventLog.reduce(
       (res, {event}: EventLog.item) =>
         res |> ReproWalletCollector.apply(event),
       ReproWalletCollector.make(),
     );

let mandatoryInputs =
  reproWalletCollector
  |> ReproWalletCollector.nonReservedOldInputs(
       AccountIndex.default,
       UserId.fromString("misthosio.id"),
     );

let keepTx = ({txId}: Network.txInput) =>
  txId != "514ec6088ef79a9c56b1530b6d0e1a47fc5e61ab74993861e315d1430de2c407";

let afterInputs = mandatoryInputs |. Belt.Set.keep(keepTx);

Js.log2(mandatoryInputs, afterInputs);

let afterInputsCount =
  afterInputs
  |> Set.toArray
  |. Array.reduce(0, (res, {txId}: Network.txInput) =>
       txId
       == "b0478fed46339ffd2d0d36b0355d782be269b0452f452d7532b8f6e1dfa8e06b" ?
         res + 1 : res
     );

let make = _children => {
  ...component,
  render: (_) =>
    <div>
      (
        text(
          "there are event log"
          ++ string_of_int(afterInputsCount)
          ++ " identical things",
        )
      )
    </div>,
};
