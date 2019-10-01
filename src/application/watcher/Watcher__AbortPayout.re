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
  ->(
      Array.forEachWithIndexU((. idx, input: Network.txInput) => {
        let keyChain =
          keyChains
          |> AccountKeyChain.Collection.lookup(
               input.coordinates |> Address.Coordinates.accountIdx,
               input.coordinates |> Address.Coordinates.keyChainIdent,
             );
        requiredSigs :=
          (requiredSigs^)
          ->(
              Map.set(
                input,
                {
                  custodians:
                    keyChain.custodianKeyChains->(List.map(fst))
                    |> List.toArray
                    |> Set.mergeMany(UserId.emptySet),
                  requiredCoSigners:
                    txWrapper.inputs->(Array.getExn(idx)).sequence
                    != Bitcoin.Transaction.defaultSequence ?
                      1 : keyChain.nCoSigners,
                },
              )
            );
      })
    );
  let anyInputNotSignable = currentCustodians =>
    (requiredSigs^)
    ->Map.valuesToArray
    ->(
        Array.someU((. {custodians, requiredCoSigners}: sigs) =>
          currentCustodians->(Set.intersect(custodians))->Set.size
          < requiredCoSigners
        )
      );
  let getResult = (systemIssuer, broadcastInputs, inputs) =>
    if (Set.intersect(broadcastInputs, inputs)->Set.size > 0) {
      Some((
        systemIssuer,
        PayoutAborted(Payout.Aborted.fromProposal(proposal)),
      ));
    } else {
      None;
    };
  let process = {
    val payoutProcesses = ref(ProcessId.makeMap());
    val completed = ref(false);
    val result = ref(None);
    val systemIssuer = ref(Bitcoin.ECPair.makeRandom());
    val custodians = ref(UserId.emptySet);
    val alreadySigned = ref(UserId.emptySet);
    val collidingProcesses = ref(ProcessId.emptySet);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | VentureCreated(event) => systemIssuer := event.systemIssuer
      | CustodianAccepted({data: {partnerId}}) =>
        custodians := (custodians^)->(Set.add(partnerId))
      | CustodianRemovalAccepted({data: {custodianId}}) =>
        custodians := (custodians^)->(Set.remove(custodianId))
      | PayoutSigned({processId: signedProcess, custodianId})
          when ProcessId.eq(signedProcess, processId) =>
        alreadySigned := (alreadySigned^)->(Set.add(custodianId))
      | PayoutAccepted({
          processId: acceptedProcess,
          data: {payoutTx: {usedInputs}},
        })
          when ProcessId.neq(processId, acceptedProcess) =>
        payoutProcesses :=
          (payoutProcesses^)
          ->(
              Map.set(
                acceptedProcess,
                usedInputs |> Set.mergeMany(Network.inputSet()),
              )
            )
      | PayoutFinalized({processId: broadcastProcess})
          when ProcessId.neq(broadcastProcess, processId) =>
        let broadcastInputs =
          (payoutProcesses^)
          ->(Map.getWithDefault(broadcastProcess, Network.inputSet()));
        switch (getResult(systemIssuer^, broadcastInputs, inputs)) {
        | Some(actualResult) =>
          result := Some(actualResult);
          collidingProcesses :=
            (collidingProcesses^)->(Set.add(broadcastProcess));
        | _ => ()
        };
      | PayoutBroadcast({processId: broadcastProcess})
          when ProcessId.eq(broadcastProcess, processId) =>
        result := None;
        completed := true;
      | PayoutBroadcastFailed({processId: broadcastProcess})
          when ProcessId.eq(broadcastProcess, processId) =>
        result := None;
        completed := true;
      | PayoutBroadcastFailed({processId}) =>
        payoutProcesses := (payoutProcesses^)->(Map.remove(processId));
        if ((collidingProcesses^)->(Set.has(processId))) {
          collidingProcesses := (collidingProcesses^)->(Set.remove(processId));
          result := None;
          (collidingProcesses^)
          ->(
              Set.forEachU((. collidingProcessId) => {
                let broadcastInputs =
                  (payoutProcesses^)->(Map.getExn(collidingProcessId));
                switch (getResult(systemIssuer^, broadcastInputs, inputs)) {
                | Some(actualResult) => result := Some(actualResult)
                | _ => ()
                };
              })
            );
        };
      | PayoutAborted({processId: abortedProcess})
          when ProcessId.eq(abortedProcess, processId) =>
        result := None;
        completed := true;
      | PayoutAborted({processId}) =>
        payoutProcesses := (payoutProcesses^)->(Map.remove(processId));
        if ((collidingProcesses^)->(Set.has(processId))) {
          collidingProcesses := (collidingProcesses^)->(Set.remove(processId));
          result := None;
          (collidingProcesses^)
          ->(
              Set.forEachU((. collidingProcessId) => {
                let broadcastInputs =
                  (payoutProcesses^)->(Map.getExn(collidingProcessId));
                switch (getResult(systemIssuer^, broadcastInputs, inputs)) {
                | Some(actualResult) => result := Some(actualResult)
                | _ => ()
                };
              })
            );
        };

      | PayoutDenied({processId: abortedProcess})
          when ProcessId.eq(abortedProcess, processId) =>
        result := None;
        completed := true;
      | PayoutDenied({processId}) =>
        payoutProcesses := (payoutProcesses^)->(Map.remove(processId));
        if ((collidingProcesses^)->(Set.has(processId))) {
          collidingProcesses := (collidingProcesses^)->(Set.remove(processId));
          result := None;
          (collidingProcesses^)
          ->(
              Set.forEachU((. collidingProcessId) => {
                let broadcastInputs =
                  (payoutProcesses^)->(Map.getExn(collidingProcessId));
                switch (getResult(systemIssuer^, broadcastInputs, inputs)) {
                | Some(actualResult) => result := Some(actualResult)
                | _ => ()
                };
              })
            );
        };
      | TransactionNoLongerDetected({txId: missingTx}) =>
        if (inputs->(Set.some(({txId}) => txId == missingTx))) {
          result :=
            Some((
              systemIssuer^,
              PayoutAborted(Payout.Aborted.fromProposal(proposal)),
            ));
        }
      | _ => ()
      };
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () =>
      (custodians^)->(Set.union(alreadySigned^)) |> anyInputNotSignable ?
        Some((
          systemIssuer^,
          PayoutAborted(Payout.Aborted.fromProposal(proposal)),
        )) :
        result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
