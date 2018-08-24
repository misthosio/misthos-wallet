open Belt;

open Event;

open PrimitiveTypes;

let make =
    (
      {
        processId: payoutProcess,
        data: {
          payoutTx:
            {usedInputs, misthosFeeAddress, changeAddress} as payoutTx,
        },
      }: Payout.Accepted.t,
      log,
    ) => {
  let (
    needsFinalizing,
    signedTxs,
    currentCustodians,
    custodiansThatSigned,
    keyChains,
    systemIssuer,
  ) =
    log
    |> EventLog.reduce(
         (
           (
             broadcast,
             txs,
             currentCustodians,
             custodiansThatSigned,
             keyChains,
             systemIssuer,
           ),
           {event},
         ) =>
           switch (event) {
           | VentureCreated({systemIssuer}) => (
               broadcast,
               txs,
               currentCustodians,
               custodiansThatSigned,
               keyChains,
               systemIssuer,
             )
           | AccountKeyChainIdentified({keyChain}) => (
               broadcast,
               txs,
               currentCustodians,
               custodiansThatSigned,
               keyChains |> AccountKeyChain.Collection.add(keyChain),
               systemIssuer,
             )
           | PayoutProposed({processId, proposerId})
               when ProcessId.eq(processId, payoutProcess) => (
               broadcast,
               [payoutTx, ...txs],
               currentCustodians,
               custodiansThatSigned |. Set.add(proposerId),
               keyChains,
               systemIssuer,
             )
           | PayoutSigned({processId, payoutTx, custodianId})
               when ProcessId.eq(processId, payoutProcess) => (
               broadcast,
               [payoutTx, ...txs],
               currentCustodians,
               custodiansThatSigned |. Set.add(custodianId),
               keyChains,
               systemIssuer,
             )
           | PayoutFinalized({processId})
               when ProcessId.eq(processId, payoutProcess) => (
               false,
               txs,
               currentCustodians,
               custodiansThatSigned,
               keyChains,
               systemIssuer,
             )
           | CustodianAccepted({data: {partnerId}}) => (
               broadcast,
               txs,
               currentCustodians |. Set.add(partnerId),
               custodiansThatSigned,
               keyChains,
               systemIssuer,
             )
           | CustodianRemovalAccepted({data: {custodianId}}) => (
               broadcast,
               txs,
               currentCustodians |. Set.remove(custodianId),
               custodiansThatSigned,
               keyChains,
               systemIssuer,
             )
           | _ => (
               broadcast,
               txs,
               currentCustodians,
               custodiansThatSigned,
               keyChains,
               systemIssuer,
             )
           },
         (
           true,
           [],
           UserId.emptySet,
           UserId.emptySet,
           AccountKeyChain.Collection.empty,
           Bitcoin.ECPair.makeRandom(),
         ),
       );
  let missingSigs =
    PayoutTransaction.missingSignatures(
      ~currentCustodians,
      ~custodiansThatSigned,
      keyChains,
      payoutTx,
    );
  let sigsReady =
      ({mandatory, additional}: PayoutTransaction.missingSignatures) =>
    mandatory |. Set.union(additional) |. Set.size == 0;
  let process = {
    val finalTransaction =
      ref(
        needsFinalizing && sigsReady(missingSigs) ?
          Some(PayoutTransaction.finalize(signedTxs)) : None,
      );
    val signedTxs = ref(signedTxs);
    val custodians = ref(currentCustodians);
    val signatures = ref(custodiansThatSigned);
    val missingSigs = ref(missingSigs);
    val delivered = ref(false);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      switch (event) {
      | PayoutFinalized({processId})
          when ProcessId.eq(processId, payoutProcess) =>
        delivered := true
      | CustodianAccepted({data: {partnerId}}) =>
        custodians := custodians^ |. Set.add(partnerId)
      | CustodianRemovalAccepted({data: {custodianId}}) =>
        custodians := custodians^ |. Set.remove(custodianId)
      | PayoutSigned({processId, custodianId, payoutTx})
          when ProcessId.eq(processId, payoutProcess) =>
        signedTxs := [payoutTx, ...signedTxs^];
        signatures := signatures^ |. Set.add(custodianId);
        missingSigs :=
          PayoutTransaction.missingSignatures(
            ~currentCustodians=custodians^,
            ~custodiansThatSigned=signatures^,
            keyChains,
            payoutTx,
          );
        finalTransaction :=
          needsFinalizing && sigsReady(missingSigs^) ?
            Some(PayoutTransaction.finalize(signedTxs^)) : None;
      | _ => ()
      };
    };
    pub processCompleted = () => needsFinalizing == false || delivered^;
    pub pendingEvent = () =>
      finalTransaction^
      |> Utils.mapOption((tx: Bitcoin.Transaction.t) =>
           (
             systemIssuer,
             PayoutFinalized(
               Event.Payout.Finalized.make(
                 ~processId=payoutProcess,
                 ~payoutTx={
                   txHex: tx |> Bitcoin.Transaction.toHex,
                   usedInputs,
                   misthosFeeAddress,
                   changeAddress,
                 },
                 ~txId=tx |> Bitcoin.Transaction.getId,
               ),
             ),
           )
         )
  };
  process;
};
