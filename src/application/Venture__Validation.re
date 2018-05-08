open PrimitiveTypes;

open WalletTypes;

open Event;

type approvalProcess = {
  supporterIds: list(userId),
  policy: Policy.t,
};

type t = {
  systemPubKey: string,
  metaPolicy: Policy.t,
  knownItems: list(string),
  currentPartners: list(userId),
  currentPartnerPubKeys: list((string, userId)),
  partnerData: list((processId, (userId, Partner.Data.t))),
  partnerRemovalData: list((processId, (userId, Partner.Removal.Data.t))),
  partnerRemovals: list((userId, processId)),
  custodianData: list((processId, (userId, Custodian.Data.t))),
  custodianRemovalData:
    list((processId, (userId, Custodian.Removal.Data.t))),
  accountCreationData: list((processId, (userId, AccountCreation.Data.t))),
  payoutData: list((processId, (userId, Payout.Data.t))),
  processes: list((processId, approvalProcess)),
  completedProcesses: list(processId),
  policies: list((string, Policy.t)),
  creatorData: Partner.Data.t,
  custodianKeyChains:
    list((userId, list((accountIdx, list(CustodianKeyChain.public))))),
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t)))),
};

let make = () => {
  systemPubKey: "",
  knownItems: [],
  currentPartners: [],
  currentPartnerPubKeys: [],
  metaPolicy: Policy.unanimous,
  partnerData: [],
  partnerRemovalData: [],
  partnerRemovals: [],
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
    lastRemoval: None,
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

let apply = ({hash, event}: EventLog.item, state) => {
  let state = {...state, knownItems: [hash, ...state.knownItems]};
  switch (event) {
  | VentureCreated({metaPolicy, systemIssuer, creatorId, creatorPubKey}) => {
      ...state,
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
      creatorData:
        Partner.Data.{
          id: creatorId,
          pubKey: creatorPubKey,
          lastRemoval: None,
        },
    }
  | PartnerProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, (supporterId, data)), ...state.partnerData],
    }
  | PartnerRemovalProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerRemovalData: [
        (processId, (supporterId, data)),
        ...state.partnerRemovalData,
      ],
    }
  | CustodianProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianData: [
        (processId, (supporterId, data)),
        ...state.custodianData,
      ],
    }
  | CustodianRemovalProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianRemovalData: [
        (processId, (supporterId, data)),
        ...state.custodianRemovalData,
      ],
    }
  | AccountCreationProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      accountCreationData: [
        (processId, (supporterId, data)),
        ...state.accountCreationData,
      ],
    }
  | PayoutProposed({supporterId, processId, data} as proposal) => {
      ...addProcess(proposal, state),
      payoutData: [(processId, (supporterId, data)), ...state.payoutData],
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
      currentPartners: [data.id, ...state.currentPartners],
      currentPartnerPubKeys: [
        (data.pubKey, data.id),
        ...state.currentPartnerPubKeys,
      ],
    }
  | PartnerRemovalAccepted({processId, data: {id}} as acceptance) =>
    let pubKey =
      state.currentPartnerPubKeys
      |> List.find(((_key, pId)) => UserId.eq(pId, id))
      |> fst;
    {
      ...completeProcess(acceptance, state),
      currentPartners: state.currentPartners |> List.filter(UserId.neq(id)),
      currentPartnerPubKeys:
        state.currentPartnerPubKeys |> List.remove_assoc(pubKey),
      partnerRemovals: [(id, processId), ...state.partnerRemovals],
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
  | IncomeDetected(_)
  | IncomeAddressExposed(_)
  | PayoutSigned(_)
  | PayoutBroadcast(_)
  | PayoutBroadcastDuplicate(_)
  | PayoutBroadcastFailed(_) => state
  };
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
      ~validateData: ('a, t) => result=defaultDataValidator,
      processName,
      dataList: list((processId, (userId, 'a))),
      {policy, supporterId, data, dependsOnProposals, dependsOnCompletions}:
        EventTypes.proposal('a),
      {policies, currentPartnerPubKeys, completedProcesses, processes} as state,
      issuerPubKey,
    ) =>
  if (dataList
      |> List.exists(((_, (previousSupporter, previousData))) =>
           UserId.eq(previousSupporter, supporterId) && previousData == data
         )) {
    BadData("This proposal already exists");
  } else if (Policy.neq(policy, policies |> List.assoc(processName))) {
    PolicyMissmatch;
  } else if (UserId.neq(
               currentPartnerPubKeys |> List.assoc(issuerPubKey),
               supporterId,
             )) {
    InvalidIssuer;
  } else {
    let proposalsThere =
      dependsOnProposals
      |> List.fold_left(
           (res, processId) =>
             (
               completedProcesses
               |> List.mem(processId)
               || processes
               |> List.mem_assoc(processId)
             )
             && res,
           true,
         );
    let completionsThere =
      dependsOnCompletions
      |> List.fold_left(
           (res, processId) =>
             completedProcesses |> List.mem(processId) && res,
           true,
         );
    switch (proposalsThere, completionsThere) {
    | (true, true) => validateData(data, state)
    | _ => DependencyNotMet
    };
  };

let validateEndorsement =
    (
      {processId, supporterId}: EventTypes.endorsement,
      {processes, currentPartnerPubKeys},
      issuerPubKey,
    ) =>
  try (
    {
      let {supporterIds} = processes |> List.assoc(processId);
      if (UserId.neq(
            currentPartnerPubKeys |> List.assoc(issuerPubKey),
            supporterId,
          )) {
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
      {processId, data, dependsOnCompletions}: EventTypes.acceptance('a),
      dataList: list((processId, (userId, 'a))),
      eq: ('a, 'a) => bool,
      {processes, currentPartners, completedProcesses},
      _issuerPubKey,
    ) =>
  try (
    {
      let {policy, supporterIds} = processes |> List.assoc(processId);
      if (eq(data, dataList |> List.assoc(processId) |> snd) == false) {
        BadData("Data doesn't match proposal");
      } else if (Policy.fulfilled(
                   ~eligable=currentPartners,
                   ~endorsed=supporterIds,
                   policy,
                 )
                 == false) {
        PolicyNotFulfilled;
      } else {
        dependsOnCompletions
        |> List.fold_left(
             (res, processId) =>
               completedProcesses |> List.mem(processId) && res,
             true,
           ) ?
          Ok : DependencyNotMet;
      };
    }
  ) {
  | Not_found => UnknownProcessId
  };

let validatePartnerData =
    ({id, lastRemoval}: Partner.Data.t, {partnerRemovals, currentPartners}) =>
  if (currentPartners |> List.mem(id)) {
    BadData("Partner already exists");
  } else {
    let partnerRemovalProcess =
      try (Some(partnerRemovals |> List.assoc(id))) {
      | Not_found => None
      };
    if (partnerRemovalProcess != lastRemoval) {
      BadData("Last removal doesn't match");
    } else {
      Ok;
    };
  };

let validatePartnerRemovalData =
    ({id}: Partner.Removal.Data.t, {currentPartners}) =>
  currentPartners |> List.mem(id) ?
    Ok :
    BadData("Partner with Id '" ++ UserId.toString(id) ++ "' doesn't exist");

let validateCustodianData =
    ({partnerApprovalProcess, partnerId}: Custodian.Data.t, {partnerData}) =>
  try (
    {
      let pData = partnerData |> List.assoc(partnerApprovalProcess) |> snd;
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
    |> List.exists(((_processId, (_, data: Custodian.Data.t))) =>
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
      {
        currentPartnerPubKeys,
        custodianData,
        completedProcesses,
        custodianKeyChains,
      },
      issuerPubKey,
    ) =>
  if (UserId.neq(
        currentPartnerPubKeys |> List.assoc(issuerPubKey),
        partnerId,
      )) {
    InvalidIssuer;
  } else {
    try (
      {
        let (process, _data) =
          custodianData
          |> List.find(((_pId, (_, data: Custodian.Data.t))) =>
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
        |> List.find(((_, (_, data: AccountCreation.Data.t))) =>
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
          keyChain.custodianKeyChains
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
      let generatedAddress = accountKeyChains |> Address.find(coordinates);
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
  | PartnerProposed(proposal) => (
      state =>
        validateProposal(
          ~validateData=validatePartnerData,
          Partner.processName,
          state.partnerData,
          proposal,
          state,
        )
    )
  | PartnerRemovalProposed(proposal) => (
      state =>
        validateProposal(
          ~validateData=validatePartnerRemovalData,
          Partner.Removal.processName,
          state.partnerRemovalData,
          proposal,
          state,
        )
    )
  | CustodianProposed(proposal) => (
      state =>
        validateProposal(
          ~validateData=validateCustodianData,
          Custodian.processName,
          state.custodianData,
          proposal,
          state,
        )
    )
  | CustodianRemovalProposed(proposal) => (
      state =>
        validateProposal(
          ~validateData=validateCustodianRemovalData,
          Custodian.Removal.processName,
          state.custodianRemovalData,
          proposal,
          state,
        )
    )
  | AccountCreationProposed(proposal) => (
      state =>
        validateProposal(
          ~validateData=validateAccountCreationData,
          AccountCreation.processName,
          state.accountCreationData,
          proposal,
          state,
        )
    )
  | PayoutProposed(proposal) => (
      state =>
        validateProposal(
          Payout.processName,
          state.payoutData,
          proposal,
          state,
        )
    )
  | PartnerEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerRemovalEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianRemovalEndorsed(endorsement) => validateEndorsement(endorsement)
  | AccountCreationEndorsed(endorsement) => validateEndorsement(endorsement)
  | PayoutEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state =>
        validateAcceptance(
          acceptance,
          state.partnerData,
          Partner.dataEq,
          state,
        )
    )
  | PartnerRemovalAccepted(acceptance) => (
      state =>
        validateAcceptance(
          acceptance,
          state.partnerRemovalData,
          Partner.Removal.dataEq,
          state,
        )
    )
  | CustodianAccepted(acceptance) => (
      state =>
        validateAcceptance(
          acceptance,
          state.custodianData,
          Custodian.dataEq,
          state,
        )
    )
  | CustodianRemovalAccepted(acceptance) => (
      state =>
        validateAcceptance(
          acceptance,
          state.custodianRemovalData,
          Custodian.Removal.dataEq,
          state,
        )
    )
  | AccountCreationAccepted(acceptance) => (
      state =>
        validateAcceptance(
          acceptance,
          state.accountCreationData,
          AccountCreation.dataEq,
          state,
        )
    )
  | PayoutAccepted(acceptance) => (
      state =>
        validateAcceptance(acceptance, state.payoutData, Payout.dataEq, state)
    )
  | CustodianKeyChainUpdated(update) =>
    validateCustodianKeyChainUpdated(update)
  | AccountKeyChainUpdated(update) => validateAccountKeyChainUpdated(update)
  | IncomeAddressExposed(event) => validateIncomeAddressExposed(event)
  | IncomeDetected(_) => ((_state, _pubKey) => Ok)
  | PayoutSigned(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcast(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcastDuplicate(_) => ((_state, _pubKey) => Ignore)
  | PayoutBroadcastFailed(_) => ((_state, _pubKey) => Ok);

let validate =
    ({knownItems} as state, {hash, event, issuerPubKey}: EventLog.item) =>
  if (knownItems |> List.mem(hash)) {
    Ignore;
  } else {
    switch (
      event,
      Event.isSystemEvent(event),
      state.currentPartnerPubKeys |> List.mem_assoc(issuerPubKey),
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
  };
