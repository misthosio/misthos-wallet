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
  partnerStoragePrefixes: list((string, string)),
  partnerPubKeys: list((string, userId)),
  partnerData: list((processId, Partner.Data.t)),
  custodianData: list((processId, Custodian.Data.t)),
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
};

let makeState = () => {
  ventureName: "",
  systemIssuer: Bitcoin.ECPair.makeRandom(),
  systemPubKey: "",
  partnerIds: [],
  partnerStoragePrefixes: [],
  partnerPubKeys: [],
  metaPolicy: Policy.absolute,
  partnerData: [],
  custodianData: [],
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
      policies:
        [
          Partner.processName,
          AccountCreation.processName,
          Custodian.processName,
          Payout.processName,
        ]
        |> List.map(n => (n, metaPolicy)),
      creatorData: Partner.Data.{id: creatorId, pubKey: creatorPubKey},
    }
  | PartnerProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, data), ...state.partnerData],
    }
  | CustodianProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianData: [(processId, data), ...state.custodianData],
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
  | CustodianEndorsed(endorsement) => endorseProcess(endorsement, state)
  | AccountCreationEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | PayoutEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      partnerIds: [data.id, ...state.partnerIds],
      partnerStoragePrefixes: [
        (data.pubKey, UserInfo.storagePrefix(~appPubKey=data.pubKey)),
        ...state.partnerStoragePrefixes,
      ],
      partnerPubKeys: [(data.pubKey, data.id), ...state.partnerPubKeys],
    }
  | AccountCreationAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      accountKeyChains: [(data.accountIdx, []), ...state.accountKeyChains],
    }
  | PayoutAccepted(acceptance) => completeProcess(acceptance, state)
  | CustodianAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      custodianKeyChains: [
        (data.partnerId, [(data.accountIdx, [])]),
        ...state.custodianKeyChains,
      ],
    }
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
  | IncomeAddressExposed(_)
  | IncomeDetected(_)
  | PayoutSigned(_)
  | PayoutBroadcast(_)
  | PayoutBroadcastFailed(_) => state
  };

type result =
  | Ok
  | InvalidIssuer
  | UnknownProcessId
  | BadData(string)
  | DuplicateEndorsement
  | PolicyMissmatch
  | PolicyNotFulfilled
  | DependencyNotMet;

let resultToString =
  fun
  | Ok => "Ok"
  | InvalidIssuer => "InvalidIssuer"
  | UnknownProcessId => "UnknownProcessId"
  | BadData(description) => "BadData(" ++ description ++ ")"
  | DuplicateEndorsement => "DuplicateEndorsement"
  | PolicyMissmatch => "PolicyMissmatch"
  | PolicyNotFulfilled => "PolicyNotFulfilled"
  | DependencyNotMet => "DependencyNotMet";

let defaultDataValidator = (_, _, _) => Ok;

let validateProposal =
    (
      ~validateData: ('a, option(processId), state) => result=defaultDataValidator,
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
    | None => validateData(data, None, state)
    | Some(processId) =>
      processes |> List.mem_assoc(processId) ?
        validateData(data, Some(processId), state) : DependencyNotMet
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
        DuplicateEndorsement;
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

let validateCustodianData =
    (data: Custodian.Data.t, dependsOn, {partnerIds, partnerData}) =>
  switch (dependsOn) {
  | None =>
    partnerIds |> List.mem(data.partnerId) ?
      Ok :
      BadData(
        "Partner with Id '"
        ++ UserId.toString(data.partnerId)
        ++ "' doesn't exist",
      )
  | Some(processId) =>
    partnerData |> List.mem_assoc(processId) ? Ok : DependencyNotMet
  };

let validateAccountCreationData =
    (
      {accountIdx}: AccountCreation.Data.t,
      _dependsOn,
      {accountCreationData},
    ) =>
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

let validateEvent =
  fun
  | VentureCreated(_) => ((_, _) => Ok)
  | PartnerProposed(proposal) =>
    validateProposal(Partner.processName, proposal)
  | CustodianProposed(proposal) =>
    validateProposal(
      ~validateData=validateCustodianData,
      Custodian.processName,
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
  | CustodianEndorsed(endorsement) => validateEndorsement(endorsement)
  | AccountCreationEndorsed(endorsement) => validateEndorsement(endorsement)
  | PayoutEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerData, state)
    )
  | CustodianAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.custodianData, state)
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
  | IncomeDetected(_) => ((_state, _pubKey) => Ok)
  | PayoutSigned(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcast(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcastFailed(_) => ((_state, _pubKey) => Ok);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  switch (
    event,
    Event.isSystemEvent(event),
    state.partnerPubKeys |> List.mem_assoc(issuerPubKey),
  ) {
  | (VentureCreated(_), _, _) => Ok
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
