open Belt;

let text = ReasonReact.string;

let reproWalletCollector =
  IncomeVenture.eventLog
  |> EventLog.reduce(
       (res, {event}: EventLog.item) =>
         res |> ReproWalletCollector.apply(event),
       ReproWalletCollector.make(),
     );

let (unused, inputs) =
  reproWalletCollector |> ReproWalletCollector.nonReservedOldInputs;

let keepTx = ({txId}: Network.txInput) =>
  txId != "514ec6088ef79a9c56b1530b6d0e1a47fc5e61ab74993861e315d1430de2c407";

let (afterUnused, afterInputs) = (
  unused |. Belt.Set.keep(keepTx),
  inputs |. Belt.Set.keep(keepTx),
);

let countInputs = set =>
  set
  |> Set.toArray
  |. Array.reduce(0, (res, {txId}: Network.txInput) =>
       txId
       == "b0478fed46339ffd2d0d36b0355d782be269b0452f452d7532b8f6e1dfa8e06b" ?
         res + 1 : res
     );

let inputs =
  Inputs.inputs
  |> Json.parseOrRaise
  |> Json.Decode.array(Network.decodeInput)
  |> Set.mergeMany(Network.inputSet());

let before = Set.eq(unused, inputs);

let after = Set.eq(afterUnused, afterInputs);

Js.log2(before, after);

Js.log2(countInputs(afterUnused), countInputs(afterInputs));

ReactDOMRe.renderToElementWithId(
  text(
    "there are equal before "
    ++ string_of_int(countInputs(afterUnused))
    ++ " identical things",
  ),
  "root",
);
