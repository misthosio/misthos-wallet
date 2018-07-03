open Belt;

open PrimitiveTypes;
open Event;

type t = {
  payoutProcesses: ProcessId.map(PayoutTransaction.t),
  oldInputs: Map.String.t(list(Network.txInput)),
};

let make = () => {
  payoutProcesses: ProcessId.makeMap(),
  oldInputs: Map.String.empty,
};

let inputsFor = (address, {oldInputs}) =>
  oldInputs |. Map.String.getWithDefault(address, []);

let apply = (event, state) =>
  switch (event) {
  | PayoutAccepted({processId, data: {payoutTx}}) => {
      ...state,
      payoutProcesses: state.payoutProcesses |. Map.set(processId, payoutTx),
    }
  | PayoutBroadcast({processId}) =>
    let payoutTx: PayoutTransaction.t =
      state.payoutProcesses |. Map.getExn(processId);
    payoutTx.usedInputs
    |. Array.reduceU(state, (. state, input: Network.txInput) =>
         {
           ...state,
           oldInputs:
             state.oldInputs
             |. Map.String.updateU(input.address, (. inputs) =>
                  [input, ...inputs |> Js.Option.getWithDefault([])] |. Some
                ),
         }
       );
  | _ => state
  };
