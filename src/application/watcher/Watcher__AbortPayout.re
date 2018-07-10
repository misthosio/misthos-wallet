open Belt;

open Event;

open PrimitiveTypes;

type sigs = {
  custodians: UserId.set,
  requiredCoSigners: int,
};

let make =
    (
      {processId, data: {payoutTx: {txHex, usedInputs}}} as proposal: Payout.Proposed.t,
      log,
    ) => {
  let inputs = usedInputs |> Set.mergeMany(Network.inputSet());
  let txWrapper = TxWrapper.make(txHex);
  let keyChains =
    log
    |> EventLog.reduce(
         (res, {event}) =>
           switch (event) {
           | AccountKeyChainIdentified({keyChain}) =>
             res |> AccountKeyChain.Collection.add(keyChain)
           | _ => res
           },
         AccountKeyChain.Collection.empty,
       );
  let requiredSigs = ref(Network.inputMap());
  usedInputs
  |. Array.forEachWithIndexU((. idx, input: Network.txInput) => {
       let keyChain =
         keyChains
         |> AccountKeyChain.Collection.lookup(
              input.coordinates |> Address.Coordinates.accountIdx,
              input.coordinates |> Address.Coordinates.keyChainIdent,
            );
       requiredSigs :=
         requiredSigs^
         |. Map.set(
              input,
              {
                custodians:
                  keyChain.custodianKeyChains
                  |. List.map(fst)
                  |> List.toArray
                  |> Set.mergeMany(UserId.emptySet),
                requiredCoSigners:
                  (txWrapper.inputs |. Array.getExn(idx)).sequence
                  != Bitcoin.Transaction.defaultSequence ?
                    1 : keyChain.nCoSigners,
              },
            );
     });
  let anyInputNotSignable = currentCustodians =>
    requiredSigs^
    |. Map.valuesToArray
    |. Array.someU((. {custodians, requiredCoSigners}: sigs) =>
         currentCustodians
         |. Set.intersect(custodians)
         |. Set.size < requiredCoSigners
       );
  let process = {
    val payoutProcesses = ref(ProcessId.makeMap());
    val completed = ref(false);
    val result = ref(None);
    val systemIssuer = ref(Bitcoin.ECPair.makeRandom());
    val custodians = ref(UserId.emptySet);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | VentureCreated(event) => systemIssuer := event.systemIssuer
      | CustodianAccepted({data: {partnerId}}) =>
        custodians := custodians^ |. Set.add(partnerId)
      | CustodianRemovalAccepted({data: {custodianId}}) =>
        custodians := custodians^ |. Set.remove(custodianId)
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
      | PayoutDenied({processId: abortedProcess})
          when ProcessId.eq(abortedProcess, processId) =>
        result := None;
        completed := true;
      | _ => ()
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () =>
      custodians^ |> anyInputNotSignable ?
        Some((
          systemIssuer^,
          PayoutAborted(Payout.Aborted.fromProposal(proposal)),
        )) :
        result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
