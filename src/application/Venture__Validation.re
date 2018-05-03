open PrimitiveTypes;

open WalletTypes;

open Event;

type approvalProcess = {
  supporterIds: list(userId),
  policy: Policy.t,
};

type state = {
  ventureName: string,
  systemIssuer: Bitcoin.ECPair.t,
  systemPubKey: string,
  metaPolicy: Policy.t,
  partnerIds: list(userId),
  partnerPubKeys: list((string, userId)),
  partnerData: list((processId, Partner.Data.t)),
  partnerRemovalData: list((processId, Partner.Removal.Data.t)),
  custodianData: list((processId, Custodian.Data.t)),
  custodianRemovalData: list((processId, Custodian.Removal.Data.t)),
  accountCreationData: list((processId, AccountCreation.Data.t)),
  payoutData: list((processId, Payout.Data.t)),
  processes: list((processId, approvalProcess)),
  completedProcesses: list(processId),
  policies: list((string, Policy.t)),
  creatorData: Partner.Data.t,
  custodianKeyChains:
    list((userId, list((accountIdx, list(CustodianKeyChain.public))))),
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
  detectedIncome: list(IncomeDetected.t),
};

let makeState = () => {
  ventureName: "",
  systemIssuer: Bitcoin.ECPair.makeRandom(),
  systemPubKey: "",
  partnerIds: [],
  partnerPubKeys: [],
  metaPolicy: Policy.unanimous,
  partnerData: [],
  partnerRemovalData: [],
  custodianData: [],
  custodianRemovalData: [],
  accountCreationData: [],
  payoutData: [],
  processes: [],
  completedProcesses: [],
  policies: [],
  creatorData: {
    id: UserId.fromString(""),
    pubKey: "",
  },
  custodianKeyChains: [],
  accountKeyChains: [],
  detectedIncome: [],
};

let addProcess =
    (
      {processId, policy, supporterId}: EventTypes.proposal('a),
      {processes} as state,
    ) => {
  ...state,
  processes: [
    (processId, {policy, supporterIds: [supporterId]}),
    ...processes,
  ],
};

let endorseProcess =
    ({processId, supporterId}: EventTypes.endorsement, {processes} as state) => {
  ...state,
  processes:
    processes
    |> List.map(((pId, process)) =>
         ProcessId.eq(pId, processId) ?
           (
             pId,
             {
               ...process,
               supporterIds: [supporterId, ...process.supporterIds],
             },
           ) :
           (pId, process)
       ),
};

let completeProcess =
    ({processId}: EventTypes.acceptance('a), {completedProcesses} as state) => {
  ...state,
  completedProcesses: [processId, ...completedProcesses],
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({
      ventureName,
      metaPolicy,
      systemIssuer,
      creatorId,
      creatorPubKey,
    }) => {
      ...state,
      ventureName,
      systemIssuer,
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy,
      policies: [
        (Partner.Removal.processName, Policy.UnanimousMinusOne),
        (Custodian.Removal.processName, Policy.UnanimousMinusOne),
        ...[
             Partner.processName,
             AccountCreation.processName,
             Custodian.processName,
             Payout.processName,
           ]
           |> List.map(n => (n, metaPolicy)),
      ],
      creatorData: Partner.Data.{id: creatorId, pubKey: creatorPubKey},
    }
  | PartnerProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, data), ...state.partnerData],
    }
  | PartnerRemovalProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerRemovalData: [(processId, data), ...state.partnerRemovalData],
    }
  | CustodianProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianData: [(processId, data), ...state.custodianData],
    }
  | CustodianRemovalProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianRemovalData: [
        (processId, data),
        ...state.custodianRemovalData,
      ],
    }
  | AccountCreationProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      accountCreationData: [(processId, data), ...state.accountCreationData],
    }
  | PayoutProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      payoutData: [(processId, data), ...state.payoutData],
    }
  | PartnerEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerRemovalEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianRemovalEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | AccountCreationEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | PayoutEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      partnerIds: [data.id, ...state.partnerIds],
      partnerPubKeys: [(data.pubKey, data.id), ...state.partnerPubKeys],
    }
  | PartnerRemovalAccepted({data: {id}} as acceptance) =>
    let pubKey =
      state.partnerPubKeys
      |> List.find(((_key, pId)) => UserId.eq(pId, id))
      |> fst;
    {
      ...completeProcess(acceptance, state),
      partnerIds: state.partnerIds |> List.filter(UserId.neq(id)),
      partnerPubKeys: state.partnerPubKeys |> List.remove_assoc(pubKey),
    };
  | AccountCreationAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      accountKeyChains: [(data.accountIdx, []), ...state.accountKeyChains],
    }
  | PayoutAccepted(acceptance) => completeProcess(acceptance, state)
  | CustodianAccepted({data: {partnerId, accountIdx}} as acceptance) =>
    let userChains =
      try (state.custodianKeyChains |> List.assoc(partnerId)) {
      | Not_found => []
      };
    let accountChains =
      try (userChains |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
      ...completeProcess(acceptance, state),
      custodianKeyChains: [
        (
          partnerId,
          [
            (accountIdx, accountChains),
            ...userChains |> List.remove_assoc(accountIdx),
          ],
        ),
        ...state.custodianKeyChains |> List.remove_assoc(partnerId),
      ],
    };
  | CustodianRemovalAccepted(acceptance) =>
    completeProcess(acceptance, state)
  | CustodianKeyChainUpdated({partnerId, keyChain}) =>
    let userChains =
      try (state.custodianKeyChains |> List.assoc(partnerId)) {
      | Not_found => []
      };
    let accountChains =
      try (userChains |> List.assoc(CustodianKeyChain.accountIdx(keyChain))) {
      | Not_found => []
      };
    {
      ...state,
      custodianKeyChains: [
        (
          partnerId,
          [
            (
              CustodianKeyChain.accountIdx(keyChain),
              [keyChain, ...accountChains],
            ),
          ],
        ),
        ...state.custodianKeyChains |> List.remove_assoc(partnerId),
      ],
    };
  | AccountKeyChainUpdated({keyChain}) =>
    let accountChains =
      try (state.accountKeyChains |> List.assoc(keyChain.accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
      accountKeyChains: [
        (
          keyChain.accountIdx,
          [(keyChain.keyChainIdx, keyChain), ...accountChains],
        ),
      ],
    };
  | IncomeDetected(event) => {
      ...state,
      detectedIncome: [event, ...state.detectedIncome],
    }
  | IncomeAddressExposed(_)
  | PayoutSigned(_)
  | PayoutBroadcast(_)
  | PayoutBroadcastDuplicate(_)
  | PayoutBroadcastFailed(_) => state
  };

type result =
  | Ok
  | Ignore
  | InvalidIssuer
  | UnknownProcessId
  | BadData(string)
  | PolicyMissmatch
  | PolicyNotFulfilled
  | DependencyNotMet;

let resultToString =
  fun
  | Ok => "Ok"
  | Ignore => "Ignore"
  | InvalidIssuer => "InvalidIssuer"
  | UnknownProcessId => "UnknownProcessId"
  | BadData(description) => "BadData(" ++ description ++ ")"
  | PolicyMissmatch => "PolicyMissmatch"
  | PolicyNotFulfilled => "PolicyNotFulfilled"
  | DependencyNotMet => "DependencyNotMet";

let defaultDataValidator = (_, _) => Ok;

let validateProposal =
    (
      ~validateData: ('a, state) => result=defaultDataValidator,
      processName,
      {policy, supporterId, data, dependsOn}: EventTypes.proposal('a),
      {policies, partnerPubKeys, processes} as state,
      issuerPubKey,
    ) =>
  if (Policy.neq(policy, policies |> List.assoc(processName))) {
    PolicyMissmatch;
  } else if (UserId.neq(
               partnerPubKeys |> List.assoc(issuerPubKey),
               supporterId,
             )) {
    InvalidIssuer;
  } else {
    switch (dependsOn) {
    | None => validateData(data, state)
    | Some(processId) =>
      processes |> List.mem_assoc(processId) ?
        validateData(data, state) : DependencyNotMet
    };
  };

let validateEndorsement =
    (
      {processId, supporterId}: EventTypes.endorsement,
      {processes, partnerPubKeys},
      issuerPubKey,
    ) =>
  try (
    {
      let {supporterIds} = processes |> List.assoc(processId);
      if (UserId.neq(partnerPubKeys |> List.assoc(issuerPubKey), supporterId)) {
        InvalidIssuer;
      } else if (supporterIds |> List.mem(supporterId)) {
        Ignore;
      } else {
        Ok;
      };
    }
  ) {
  | Not_found => UnknownProcessId
  };

let validateAcceptance =
    (
      {processId, data, dependsOn}: EventTypes.acceptance('a),
      dataList: list((processId, 'a)),
      {processes, partnerIds, completedProcesses},
      _issuerPubKey,
    ) =>
  try (
    {
      let {policy, supporterIds} = processes |> List.assoc(processId);
      if (data != (dataList |> List.assoc(processId))) {
        BadData("Data doesn't match proposal");
      } else if (Policy.fulfilled(
                   ~eligable=partnerIds,
                   ~endorsed=supporterIds,
                   policy,
                 )
                 == false) {
        PolicyNotFulfilled;
      } else {
        switch (dependsOn) {
        | None => Ok
        | Some(processId) =>
          completedProcesses |> List.mem(processId) ? Ok : DependencyNotMet
        };
      };
    }
  ) {
  | Not_found => UnknownProcessId
  };

let validatePartnerData = ({id}: Partner.Data.t, {partnerIds}) =>
  partnerIds |> List.mem(id) ? Ignore : Ok;

let validatePartnerRemovalData =
    ({id}: Partner.Removal.Data.t, {partnerIds}) =>
  partnerIds |> List.mem(id) ?
    Ok :
    BadData("Partner with Id '" ++ UserId.toString(id) ++ "' doesn't exist");

let validateCustodianData =
    ({partnerApprovalProcess, partnerId}: Custodian.Data.t, {partnerData}) =>
  try (
    {
      let pData = partnerData |> List.assoc(partnerApprovalProcess);
      UserId.eq(pData.id, partnerId) ?
        Ok : BadData("Partner with Id 'custodian.id' doesn't exist");
    }
  ) {
  | Not_found => BadData("Partner with Id 'custodian.id' doesn't exist")
  };

let validateCustodianRemovalData =
    ({custodianId, accountIdx}: Custodian.Removal.Data.t, {custodianData}) =>
  try (
    custodianData
    |> List.exists(((_processId, data: Custodian.Data.t)) =>
         UserId.eq(data.partnerId, custodianId)
         && AccountIndex.eq(accountIdx, data.accountIdx)
       ) ?
      Ok : raise(Not_found)
  ) {
  | Not_found =>
    BadData(
      "Partner with Id '"
      ++ UserId.toString(custodianId)
      ++ "' is not a custodian of account with index "
      ++ string_of_int(AccountIndex.toInt(accountIdx)),
    )
  };

let validateAccountCreationData =
    ({accountIdx}: AccountCreation.Data.t, {accountCreationData}) =>
  accountIdx |> AccountIndex.toInt == (accountCreationData |> List.length) ?
    Ok : BadData("Bad Account Index");

let validateCustodianKeyChainUpdated =
    (
      {partnerId, keyChain}: CustodianKeyChainUpdated.t,
      {partnerPubKeys, custodianData, completedProcesses, custodianKeyChains},
      issuerPubKey,
    ) =>
  if (UserId.neq(partnerPubKeys |> List.assoc(issuerPubKey), partnerId)) {
    InvalidIssuer;
  } else {
    try (
      {
        let (process, _data) =
          custodianData
          |> List.find(((_pId, data: Custodian.Data.t)) =>
               data.partnerId == partnerId
               && data.accountIdx == CustodianKeyChain.accountIdx(keyChain)
             );
        if (completedProcesses |> List.mem(process)) {
          if (custodianKeyChains
              |> List.assoc(partnerId)
              |> List.assoc(CustodianKeyChain.accountIdx(keyChain))
              |>
              List.length != (
                               CustodianKeyChain.keyChainIdx(keyChain)
                               |> CustodianKeyChainIndex.toInt
                             )) {
            BadData("Bad KeyChainIndex");
          } else {
            Ok;
          };
        } else {
          BadData("Custodian isn't accepted yet");
        };
      }
    ) {
    | Not_found => BadData("Custodian doesn't exist")
    };
  };

let validateAccountKeyChainUpdated =
    (
      {keyChain}: AccountKeyChainUpdated.t,
      {
        accountCreationData,
        completedProcesses,
        custodianKeyChains,
        accountKeyChains,
      },
      _issuerPubKey,
    ) =>
  try (
    {
      let (pId, _) =
        accountCreationData
        |> List.find(((_, data: AccountCreation.Data.t)) =>
             data.accountIdx == keyChain.accountIdx
           );
      if (completedProcesses |> List.mem(pId)) {
        if (accountKeyChains
            |> List.assoc(keyChain.accountIdx)
            |>
            List.length != (keyChain.keyChainIdx |> AccountKeyChainIndex.toInt)) {
          BadData("Bad KeyChainIndex");
        } else {
          let accountIdx = keyChain.accountIdx;
          keyChain
          |> AccountKeyChain.custodianKeyChains
          |> List.map(((partnerId, keyChain)) =>
               try (
                 {
                   let latestKeyChain =
                     custodianKeyChains
                     /* list((userId, list((int, list(CustodianKeyChain.public))))), */
                     |> List.assoc(partnerId)
                     |> List.assoc(accountIdx)
                     |> List.sort((keysA, keysB) =>
                          compare(
                            keysA |> CustodianKeyChain.keyChainIdx,
                            keysB |> CustodianKeyChain.keyChainIdx,
                          )
                        )
                     |> List.rev
                     |> List.hd;
                   keyChain == latestKeyChain ?
                     Ok : BadData("Bad CustodianKeyChain");
                 }
               ) {
               | Not_found => BadData("Bad CustodianKeyChain")
               }
             )
          |> List.fold_left((result, test) => test == Ok ? result : test, Ok);
        };
      } else {
        BadData("Account doesn't exist");
      };
    }
  ) {
  | Not_found => BadData("Account doesn't exist")
  };

let validateIncomeAddressExposed =
    (
      {coordinates, address}: IncomeAddressExposed.t,
      {accountKeyChains},
      _issuerPubKey,
    ) =>
  try (
    {
      let generatedAddress =
        accountKeyChains |> AccountKeyChain.find(coordinates);
      if (address == generatedAddress.address) {
        Ok;
      } else {
        BadData("Unknown Address");
      };
    }
  ) {
  | Not_found => BadData("Unknown Address")
  };

let validateIncomeDetected =
    (
      {address: a, amount: am, txId: id}: IncomeDetected.t,
      {detectedIncome},
      _issuerPubKey,
    ) =>
  detectedIncome
  |> List.exists(({address, amount, txId}: IncomeDetected.t) =>
       address == a && BTC.comparedTo(am, amount) == 0 && id == txId
     ) ?
    Ignore : Ok;

let validateEvent =
  fun
  | VentureCreated(_) => ((_, _) => Ok)
  | PartnerProposed(proposal) =>
    validateProposal(
      ~validateData=validatePartnerData,
      Partner.processName,
      proposal,
    )
  | PartnerRemovalProposed(proposal) =>
    validateProposal(
      ~validateData=validatePartnerRemovalData,
      Partner.Removal.processName,
      proposal,
    )
  | CustodianProposed(proposal) =>
    validateProposal(
      ~validateData=validateCustodianData,
      Custodian.processName,
      proposal,
    )
  | CustodianRemovalProposed(proposal) =>
    validateProposal(
      ~validateData=validateCustodianRemovalData,
      Custodian.Removal.processName,
      proposal,
    )
  | AccountCreationProposed(proposal) =>
    validateProposal(
      ~validateData=validateAccountCreationData,
      AccountCreation.processName,
      proposal,
    )
  | PayoutProposed(proposal) =>
    validateProposal(Payout.processName, proposal)
  | PartnerEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerRemovalEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianRemovalEndorsed(endorsement) => validateEndorsement(endorsement)
  | AccountCreationEndorsed(endorsement) => validateEndorsement(endorsement)
  | PayoutEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerData, state)
    )
  | PartnerRemovalAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerRemovalData, state)
    )
  | CustodianAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.custodianData, state)
    )
  | CustodianRemovalAccepted(acceptance) => (
      state =>
        validateAcceptance(acceptance, state.custodianRemovalData, state)
    )
  | AccountCreationAccepted(acceptance) => (
      state =>
        validateAcceptance(acceptance, state.accountCreationData, state)
    )
  | PayoutAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.payoutData, state)
    )
  | CustodianKeyChainUpdated(update) =>
    validateCustodianKeyChainUpdated(update)
  | AccountKeyChainUpdated(update) => validateAccountKeyChainUpdated(update)
  | IncomeAddressExposed(event) => validateIncomeAddressExposed(event)
  | IncomeDetected(event) => validateIncomeDetected(event)
  | PayoutSigned(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcast(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcastDuplicate(_) => ((_state, _pubKey) => Ignore)
  | PayoutBroadcastFailed(_) => ((_state, _pubKey) => Ok);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  switch (
    event,
    Event.isSystemEvent(event),
    state.partnerPubKeys |> List.mem_assoc(issuerPubKey),
  ) {
  | (VentureCreated(_), _, _) =>
    UserId.eq(state.creatorData.id, UserId.fromString("")) ?
      Ok : BadData("Venture is already created")
  | (PartnerProposed(event), false, false)
      when
        event.data == state.creatorData
        && issuerPubKey == state.creatorData.pubKey
        && state.partnerData
        |> List.length == 0 =>
    Ok
  | (_, false, false) => InvalidIssuer
  | (_, true, _) when issuerPubKey != state.systemPubKey => InvalidIssuer
  | (PartnerAccepted(event), true, false)
      when
        event.data == state.creatorData
        && state.partnerData
        |> List.length == 1 =>
    Ok
  | _ => validateEvent(event, state, issuerPubKey)
  };
