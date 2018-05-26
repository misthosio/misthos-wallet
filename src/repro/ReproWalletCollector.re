open Belt;

open PrimitiveTypes;

open Event;

type t = {
  unused: Network.inputSet,
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
};

let inputs =
  Inputs.inputs
  |> Json.parseOrRaise
  |> Json.Decode.array(Network.decodeInput)
  |> Set.mergeMany(Network.inputSet());

let nonReservedOldInputs = ({unused}) => {
  Js.log2("eq before?", Set.eq(unused, inputs));
  (
    unused
    |. Belt.Set.keepU((. i: Network.txInput) =>
         if (i.txId
             == "35815aaadec8a110391de8ae2e8c304e3e6084d3cd1344d8155a2293ee54324b"
             ||
             i.txId == "d029a186f3d3124aca7fdc95d085ce25e0519918bf63ecb32cdfbb1da3268d8c") {
           false;
         } else {
           true;
         }
       ),
    inputs
    |. Belt.Set.keepU((. i: Network.txInput) =>
         if (i.txId
             == "35815aaadec8a110391de8ae2e8c304e3e6084d3cd1344d8155a2293ee54324b"
             ||
             i.txId == "d029a186f3d3124aca7fdc95d085ce25e0519918bf63ecb32cdfbb1da3268d8c") {
           false;
         } else {
           true;
         }
       ),
  );
};

let make = () => {
  unused: Network.inputSet(),
  payoutProcesses: ProcessId.makeMap(),
};

let apply = ({address, txId, txOutputN, amount}: IncomeEvent.t, state) => {
  ...state,
  unused:
    state.unused
    |. Set.add({
         txId,
         txOutputN,
         address,
         value: amount,
         nCoSigners: 2,
         nPubKeys: 3,
       }),
};
