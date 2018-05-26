open Belt;

let filterOne = set =>
  set
  |. Belt.Set.keepU((. i: IncomeEvent.txInput) =>
       if (i.txId
           == "35815aaadec8a110391de8ae2e8c304e3e6084d3cd1344d8155a2293ee54324b"
           ||
           i.txId == "d029a186f3d3124aca7fdc95d085ce25e0519918bf63ecb32cdfbb1da3268d8c") {
         false;
       } else {
         true;
       }
     );

let text = ReasonReact.string;

let smallLog =
  Income.income |> Json.parseOrRaise |> Json.Decode.array(IncomeEvent.decode);

let unused = IncomeEvent.inputSet();

let unused =
  smallLog
  |. Array.reduce(
       IncomeEvent.inputSet(),
       (unused, {address, txId, txOutputN}: IncomeEvent.t) =>
       unused
       |. Set.add({txId, txOutputN, address, nCoSigners: 2, nPubKeys: 3})
     );

let filteredUnused = filterOne(unused);

let filterTwo = ({txId}: IncomeEvent.txInput) =>
  txId != "514ec6088ef79a9c56b1530b6d0e1a47fc5e61ab74993861e315d1430de2c407";

let unusedAfter = unused |. Belt.Set.keep(filterTwo);

let countInputs = set =>
  set
  |> Set.toArray
  |. Array.reduce(0, (res, {txId}: IncomeEvent.txInput) =>
       txId
       == "b0478fed46339ffd2d0d36b0355d782be269b0452f452d7532b8f6e1dfa8e06b" ?
         res + 1 : res
     );

ReactDOMRe.renderToElementWithId(
  text("one file" ++ string_of_int(countInputs(unusedAfter)) ++ " yup"),
  "root",
);
