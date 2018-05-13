open PrimitiveTypes;

open WalletTypes;

open Event;

type approvalProcess = {
  supporterIds: list(userId),
  policy: Policy.t,
};

type t = {
  accountValidator: AccountValidator.t,
  custodianValidator: CustodianValidator.t,
  custodianKeyChainValidator: CustodianKeyChainValidator.t,
  accountKeyChainValidator: AccountKeyChainValidator.t,
  systemPubKey: string,
  metaPolicy: Policy.t,
  knownItems: list(string),
  currentPartners: list(userId),
  currentPartnerPubKeys: list((string, userId)),
  partnerData: list((processId, (userId, Partner.Data.t))),
  partnerAccepted: list((userId, processId)),
  partnerRemovalData: list((processId, (userId, Partner.Removal.Data.t))),
  partnerRemovals: list((userId, processId)),
  custodianData: list((processId, (userId, Custodian.Data.t))),
  custodianRemovalData:
    list((processId, (userId, Custodian.Removal.Data.t))),
  custodianRemovals: list((userId, processId)),
  currentCustodians: list((accountIdx, list(userId))),
  accountCreationData: list((processId, (userId, AccountCreation.Data.t))),
  payoutData: list((processId, (userId, Payout.Data.t))),
  processes: list((processId, approvalProcess)),
  completedProcesses: list(processId),
  policies: list((string, Policy.t)),
  creatorData: Partner.Data.t,
  custodianKeyChains:
    list((userId, list((accountIdx, list(CustodianKeyChain.public))))),
  accountKeyChains: AccountKeyChain.Collection.t,
};

let make = () => {
  accountValidator: AccountValidator.make(),
  custodianValidator: CustodianValidator.make(),
  custodianKeyChainValidator: CustodianKeyChainValidator.make(),
  accountKeyChainValidator: AccountKeyChainValidator.make(),
  systemPubKey: "",
  knownItems: [],
  currentPartners: [],
  currentPartnerPubKeys: [],
  metaPolicy: Policy.unanimous,
  partnerData: [],
  partnerAccepted: [],
  partnerRemovalData: [],
  partnerRemovals: [],
  custodianData: [],
  custodianRemovalData: [],
  custodianRemovals: [],
  currentCustodians: [],
  accountCreationData: [],
  payoutData: [],
  processes: [],
  completedProcesses: [],
  policies: [],
  creatorData: {
    id: UserId.fromString(""),
    pubKey: "",
    lastPartnerRemovalProcess: None,
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
  let state = {
    ...state,
    knownItems: [hash, ...state.knownItems],
    accountValidator:
      state.accountValidator |> AccountValidator.update(event),
    custodianValidator:
      state.custodianValidator |> CustodianValidator.update(event),
    custodianKeyChainValidator:
      state.custodianKeyChainValidator
      |> CustodianKeyChainValidator.update(event),
    accountKeyChainValidator:
      state.accountKeyChainValidator |> AccountKeyChainValidator.update(event),
  };
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
          lastPartnerRemovalProcess: None,
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
  | PartnerRejected(_)
  | PartnerRemovalRejected(_)
  | CustodianRejected(_)
  | CustodianRemovalRejected(_)
  | AccountCreationRejected(_)
  | PayoutRejected(_) => state
  | PartnerEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerRemovalEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianRemovalEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | AccountCreationEndorsed(endorsement) =>
    endorseProcess(endorsement, state)
  | PayoutEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerAccepted({processId, data} as acceptance) => {
      ...completeProcess(acceptance, state),
      currentPartners: [data.id, ...state.currentPartners],
      currentPartnerPubKeys: [
        (data.pubKey, data.id),
        ...state.currentPartnerPubKeys,
      ],
      partnerAccepted: [(data.id, processId), ...state.partnerAccepted],
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
      currentCustodians: [(data.accountIdx, []), ...state.currentCustodians],
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
      currentCustodians: [
        (
          accountIdx,
          [partnerId, ...state.currentCustodians |> List.assoc(accountIdx)],
        ),
        ...state.currentCustodians |> List.remove_assoc(accountIdx),
      ],
    };
  | CustodianRemovalAccepted(
      {processId, data: {custodianId, accountIdx}} as acceptance,
    ) => {
      ...completeProcess(acceptance, state),
      custodianRemovals: [
        (custodianId, processId),
        ...state.custodianRemovals,
      ],
      currentCustodians: [
        (
          accountIdx,
          state.currentCustodians
          |> List.assoc(accountIdx)
          |> List.filter(UserId.neq(custodianId)),
        ),
        ...state.currentCustodians |> List.remove_assoc(accountIdx),
      ],
    }
  | CustodianKeyChainUpdated({custodianId, keyChain}) =>
    let userChains =
      try (state.custodianKeyChains |> List.assoc(custodianId)) {
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
          custodianId,
          [
            (
              CustodianKeyChain.accountIdx(keyChain),
              [keyChain, ...accountChains],
            ),
          ],
        ),
        ...state.custodianKeyChains |> List.remove_assoc(custodianId),
      ],
    };
  | AccountKeyChainActivated(_) => state
  | AccountKeyChainIdentified({keyChain}) => {
      ...state,
      accountKeyChains:
        state.accountKeyChains |> AccountKeyChain.Collection.add(keyChain),
    }
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
  | AlreadyEndorsed
  | PolicyMissmatch
  | PolicyNotFulfilled
  | DependencyNotMet
  | BadData(string);

let resultToString =
  fun
  | Ok => "Ok"
  | Ignore => "Ignore"
  | InvalidIssuer => "InvalidIssuer"
  | UnknownProcessId => "UnknownProcessId"
  | AlreadyEndorsed => "AlreadyEndorsed"
  | PolicyMissmatch => "PolicyMissmatch"
  | PolicyNotFulfilled => "PolicyNotFulfilled"
  | DependencyNotMet => "DependencyNotMet"
  | BadData(description) => "BadData('" ++ description ++ "')";

let accountExists = (accountIdx, {accountValidator}) =>
  accountValidator.exists(accountIdx) ?
    Ok : BadData("Account doesn't exist");

let isCustodian = (accountIdx, custodian, {custodianValidator}) =>
  custodian |> custodianValidator.isCustodian(accountIdx) ?
    Ok : BadData("Not a custodian");

let currentCustodians = (accountIdx, custodians, {custodianValidator}) =>
  custodians |> custodianValidator.areCurrent(accountIdx) ?
    Ok : BadData("Custodians aren't current");

let custodianKeyChainsExist =
    (accountIdx, keyChains, {custodianKeyChainValidator}) =>
  keyChains |> custodianKeyChainValidator.allExist(accountIdx) ?
    Ok : BadData("Bad CustodianKeyChain");

let accountKeyChainIdentified =
    (accountIdx, identifier, {accountKeyChainValidator}) =>
  identifier |> accountKeyChainValidator.exists(accountIdx) ?
    Ok : BadData("Unknown AccountKeyChain identifier");

let activationSequenceInOrder =
    (custodianId, identifier, sequence, {accountKeyChainValidator}) =>
  sequence |> accountKeyChainValidator.inOrder(custodianId, identifier) ?
    Ok : BadData("AccountKeyChain sequence out of order");

let test = (test, state) => (state |> test, state);

let andThen = (test, (res, state)) =>
  switch (res) {
  | Ok => (state |> test, state)
  | _ => (res, state)
  };

let returnResult = ((res, _)) => res;

let defaultDataValidator = (_, _) => Ok;

let validateProposal =
    (
      ~validateData: ('a, t) => result=defaultDataValidator,
      processName,
      dataList: list((processId, (userId, 'a))),
      {policy, supporterId, data, dependsOnProposals, dependsOnCompletions}:
        EventTypes.proposal('a),
      {policies, completedProcesses, processes} as state,
      issuerId,
    ) =>
  if (dataList
      |> List.exists(((_, (previousSupporter, previousData))) =>
           UserId.eq(previousSupporter, supporterId) && previousData == data
         )) {
    BadData("This proposal already exists");
  } else if (Policy.neq(policy, policies |> List.assoc(processName))) {
    PolicyMissmatch;
  } else if (UserId.neq(issuerId, supporterId)) {
    InvalidIssuer;
  } else {
    let proposalsThere =
      dependsOnProposals
      |> Array.fold_left(
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
      |> Array.fold_left(
           (res, processId) =>
             completedProcesses |> List.mem(processId) && res,
           true,
         );
    switch (proposalsThere, completionsThere) {
    | (true, true) => validateData(data, state)
    | _ => DependencyNotMet
    };
  };

let validateRejection =
    ({processId, rejectorId}: EventTypes.rejection, {processes}, issuerId) =>
  try (
    {
      let {supporterIds} = processes |> List.assoc(processId);
      if (UserId.neq(issuerId, rejectorId)) {
        InvalidIssuer;
      } else if (supporterIds |> List.mem(rejectorId)) {
        AlreadyEndorsed;
      } else {
        Ok;
      };
    }
  ) {
  | Not_found => UnknownProcessId
  };

let validateEndorsement =
    (
      {processId, supporterId}: EventTypes.endorsement,
      {processes},
      issuerId,
    ) =>
  if (processes |> List.mem_assoc(processId)) {
    UserId.neq(issuerId, supporterId) ? InvalidIssuer : Ok;
  } else {
    UnknownProcessId;
  };

let validateAcceptance =
    (
      {processId, data, dependsOnCompletions}: EventTypes.acceptance('a),
      dataList: list((processId, (userId, 'a))),
      eq: ('a, 'a) => bool,
      {processes, currentPartners, completedProcesses},
      _issuerId,
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
        |> Array.fold_left(
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
    (
      {id, lastPartnerRemovalProcess}: Partner.Data.t,
      {partnerRemovals, currentPartners},
    ) =>
  if (currentPartners |> List.mem(id)) {
    BadData("Partner already exists");
  } else {
    let partnerRemovalProcess =
      try (Some(partnerRemovals |> List.assoc(id))) {
      | Not_found => None
      };
    if (partnerRemovalProcess != lastPartnerRemovalProcess) {
      BadData("Last removal doesn't match");
    } else {
      Ok;
    };
  };

let validatePartnerRemovalData =
    (
      {id, lastPartnerProcess}: Partner.Removal.Data.t,
      {partnerAccepted, currentPartners},
    ) =>
  if (currentPartners |> List.mem(id) == false) {
    BadData("Partner with Id '" ++ UserId.toString(id) ++ "' doesn't exist");
  } else {
    try (
      {
        let partnerProcess = partnerAccepted |> List.assoc(id);
        if (ProcessId.eq(partnerProcess, lastPartnerProcess)) {
          Ok;
        } else {
          BadData("lastPartnerProcess doesn't match");
        };
      }
    ) {
    | Not_found => BadData("lastPartnerProcess doesn't match")
    };
  };

let validateCustodianData =
    (
      {
        accountIdx,
        lastCustodianRemovalProcess,
        partnerApprovalProcess,
        partnerId,
      }: Custodian.Data.t,
      {custodianRemovals, accountCreationData, partnerData},
    ) =>
  if (accountCreationData
      |>
      List.exists(((_, (_, accountData: AccountCreation.Data.t))) =>
        AccountIndex.eq(accountData.accountIdx, accountIdx)
      ) == false) {
    BadData("account doesn't exist");
  } else {
    try (
      {
        let pData = partnerData |> List.assoc(partnerApprovalProcess) |> snd;
        if (UserId.neq(pData.id, partnerId)) {
          BadData("Partner approval process doesn't match user id");
        } else {
          let custodianRemovalProcess =
            try (Some(custodianRemovals |> List.assoc(partnerId))) {
            | Not_found => None
            };
          if (custodianRemovalProcess != lastCustodianRemovalProcess) {
            BadData("Last removal doesn't match");
          } else {
            Ok;
          };
        };
      }
    ) {
    | Not_found => BadData("partner approval process doesn't exist")
    };
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
      {custodianApprovalProcess, custodianId, keyChain}: CustodianKeyChainUpdated.t,
      {
        accountCreationData,
        custodianData,
        completedProcesses,
        custodianKeyChains,
      },
      issuerId,
    ) =>
  if (UserId.neq(issuerId, custodianId)) {
    InvalidIssuer;
  } else {
    let accountIdx = keyChain |> CustodianKeyChain.accountIdx;
    let pId =
      try (
        accountCreationData
        |> List.find(((_, (_, data: AccountCreation.Data.t))) =>
             AccountIndex.eq(data.accountIdx, accountIdx)
           )
        |> fst
      ) {
      | Not_found => ProcessId.fromString("impossible")
      };
    if (completedProcesses |> List.mem(pId) == false) {
      BadData("Account doesn't exist");
    } else if (custodianData
               |> List.mem_assoc(custodianApprovalProcess) == false
               || completedProcesses
               |> List.mem(custodianApprovalProcess) == false) {
      BadData("Bad custodianApprovalProcess");
    } else {
      let (_, custodianData: Custodian.Data.t) =
        custodianData |> List.assoc(custodianApprovalProcess);
      if (UserId.neq(custodianData.partnerId, custodianId)) {
        BadData("CustodianApprovalProcess is for another partner");
      } else if (CustodianKeyChainIndex.neq(
                   keyChain |> CustodianKeyChain.keyChainIdx,
                   custodianKeyChains
                   |> List.assoc(custodianId)
                   |> List.assoc(accountIdx)
                   |> List.length
                   |> CustodianKeyChainIndex.fromInt,
                 )) {
        BadData("CustodianKeyChainIndex isn't in order");
      } else {
        Ok;
      };
    };
  };

let validateAccountKeyChainIdentified =
    (
      {keyChain: {accountIdx, custodianKeyChains} as keyChain}: AccountKeyChainIdentified.t,
      state,
      _issuerId,
    ) =>
  AccountKeyChain.isConsistent(keyChain) == false ?
    BadData("Inconsistent AccountKeyChain") :
    state
    |> test(accountExists(accountIdx))
    |> andThen(
         custodianKeyChains |> List.map(fst) |> currentCustodians(accountIdx),
       )
    |> andThen(custodianKeyChains |> custodianKeyChainsExist(accountIdx))
    |> returnResult;

let validateAccountKeyChainActivated =
    (
      {accountIdx, custodianId, identifier, sequence}: AccountKeyChainActivated.t,
      state,
      issuerId,
    ) =>
  UserId.neq(issuerId, custodianId) ?
    InvalidIssuer :
    state
    |> test(accountExists(accountIdx))
    |> andThen(custodianId |> isCustodian(accountIdx))
    |> andThen(identifier |> accountKeyChainIdentified(accountIdx))
    |> andThen(
         sequence |> activationSequenceInOrder(custodianId, identifier),
       )
    |> returnResult;

let validateIncomeAddressExposed =
    (
      {coordinates, address}: IncomeAddressExposed.t,
      {accountKeyChains},
      _issuerId,
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
  | PartnerRejected(rejection) => validateRejection(rejection)
  | PartnerRemovalRejected(rejection) => validateRejection(rejection)
  | CustodianRejected(rejection) => validateRejection(rejection)
  | CustodianRemovalRejected(rejection) => validateRejection(rejection)
  | AccountCreationRejected(rejection) => validateRejection(rejection)
  | PayoutRejected(rejection) => validateRejection(rejection)
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
  | AccountKeyChainIdentified(update) =>
    validateAccountKeyChainIdentified(update)
  | AccountKeyChainActivated(update) =>
    validateAccountKeyChainActivated(update)
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
    | (_, true, _) =>
      validateEvent(event, state, UserId.fromString("system"))
    | _ =>
      validateEvent(
        event,
        state,
        state.currentPartnerPubKeys |> List.assoc(issuerPubKey),
      )
    };
  };
