open Belt;

open Event;

open PrimitiveTypes;

let make =
    (
      {processId, data: {payoutTx: {usedInputs}}} as proposal: Payout.Proposed.t,
      log,
    ) => {
  let inputs = usedInputs |> Set.mergeMany(Network.inputSet());
  let process = {
    val payoutProcesses = ref(ProcessId.makeMap());
    val completed = ref(false);
    val result = ref(None);
    val systemIssuer = ref(Bitcoin.ECPair.makeRandom());
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | VentureCreated(event) => systemIssuer := event.systemIssuer
      | PayoutAccepted({
          processId: acceptedProcess,
          data: {payoutTx: {usedInputs}},
        })
          when ProcessId.neq(processId, acceptedProcess) =>
        payoutProcesses :=
          payoutProcesses^
          |. Map.set(
               acceptedProcess,
               usedInputs |> Set.mergeMany(Network.inputSet()),
             )
      | PayoutFinalized({processId: broadcastProcess})
          when ProcessId.neq(broadcastProcess, processId) =>
        let broadcastInputs =
          payoutProcesses^ |. Map.getExn(broadcastProcess);
        if (Set.intersect(broadcastInputs, inputs) |. Set.size > 0) {
          result :=
            Some((
              systemIssuer^,
              PayoutAborted(Payout.Aborted.fromProposal(proposal)),
            ));
        };
        payoutProcesses := payoutProcesses^ |. Map.remove(broadcastProcess);
      | PayoutBroadcast({processId: broadcastProcess})
          when ProcessId.eq(broadcastProcess, processId) =>
        result := None;
        completed := true;
      | PayoutBroadcastFailed({processId: broadcastProcess})
          when ProcessId.eq(broadcastProcess, processId) =>
        result := None;
        completed := true;
      | PayoutAborted({processId: abortedProcess})
          when ProcessId.eq(abortedProcess, processId) =>
        result := None;
        completed := true;
      | _ => ()
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
