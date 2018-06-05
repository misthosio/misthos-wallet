open PrimitiveTypes;

open WalletTypes;

open Event;

module ItemsSet = Belt.Set.String;

type t = {
  accountValidator: AccountValidator.t,
  custodianValidator: CustodianValidator.t,
  custodianKeyChainValidator: CustodianKeyChainValidator.t,
  accountKeyChainValidator: AccountKeyChainValidator.t,
  processValidator: ProcessValidator.t,
  systemPubKey: string,
  knownItems: ItemsSet.t,
  currentPartners: UserId.set,
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
  processValidator: ProcessValidator.make(),
  systemPubKey: "",
  knownItems: ItemsSet.empty,
  currentPartners: UserId.emptySet,
  currentPartnerPubKeys: [],
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
  creatorData: {
    id: UserId.fromString(""),
    pubKey: None,
    lastPartnerRemovalProcess: None,
  },
  custodianKeyChains: [],
  accountKeyChains: AccountKeyChain.Collection.empty,
};

let apply = ({hash, event}: EventLog.item, state) => {
  let state = {
    ...state,
    knownItems: state.knownItems |. ItemsSet.add(hash),
    accountValidator:
      state.accountValidator |> AccountValidator.update(event),
    processValidator:
      state.processValidator |> ProcessValidator.update(event),
    custodianValidator:
      state.custodianValidator |> CustodianValidator.update(event),
    custodianKeyChainValidator:
      state.custodianKeyChainValidator
      |> CustodianKeyChainValidator.update(event),
    accountKeyChainValidator:
      state.accountKeyChainValidator |> AccountKeyChainValidator.update(event),
  };
  switch (event) {
  | VentureCreated({systemIssuer, creatorId, creatorPubKey}) => {
      ...state,
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      creatorData:
        Partner.Data.{
          id: creatorId,
          pubKey: Some(creatorPubKey),
          lastPartnerRemovalProcess: None,
        },
    }
  | PartnerProposed({proposerId, processId, data}) => {
      ...state,
      partnerData: [(processId, (proposerId, data)), ...state.partnerData],
    }
  | PartnerRemovalProposed({proposerId, processId, data}) => {
      ...state,
      partnerRemovalData: [
        (processId, (proposerId, data)),
        ...state.partnerRemovalData,
      ],
    }
  | CustodianProposed({proposerId, processId, data}) => {
      ...state,
      custodianData: [
        (processId, (proposerId, data)),
        ...state.custodianData,
      ],
    }
  | CustodianRemovalProposed({proposerId, processId, data}) => {
      ...state,
      custodianRemovalData: [
        (processId, (proposerId, data)),
        ...state.custodianRemovalData,
      ],
    }
  | AccountCreationProposed({proposerId, processId, data}) => {
      ...state,
      accountCreationData: [
        (processId, (proposerId, data)),
        ...state.accountCreationData,
      ],
    }
  | PayoutProposed({proposerId, processId, data}) => {
      ...state,
      payoutData: [(processId, (proposerId, data)), ...state.payoutData],
    }
  | PartnerRejected(_)
  | PartnerRemovalRejected(_)
  | CustodianRejected(_)
  | CustodianRemovalRejected(_)
  | AccountCreationRejected(_)
  | PayoutRejected(_) => state
  | PartnerEndorsed(_)
  | PartnerRemovalEndorsed(_)
  | CustodianEndorsed(_)
  | CustodianRemovalEndorsed(_)
  | AccountCreationEndorsed(_)
  | PayoutEndorsed(_) => state
  | PartnerAccepted({processId, data}) => {
      ...state,
      currentPartners: state.currentPartners |. Belt.Set.add(data.id),
      currentPartnerPubKeys:
        data.pubKey
        |> Utils.mapOption(pubKey =>
             [(pubKey, data.id), ...state.currentPartnerPubKeys]
           )
        |> Js.Option.getWithDefault(state.currentPartnerPubKeys),
      partnerAccepted: [(data.id, processId), ...state.partnerAccepted],
    }
  | PartnerRemovalAccepted({processId, data: {id}}) =>
    let pubKey =
      state.currentPartnerPubKeys
      |> List.find(((_key, pId)) => UserId.eq(pId, id))
      |> fst;
    {
      ...state,
      currentPartners: state.currentPartners |. Belt.Set.remove(id),
      currentPartnerPubKeys:
        state.currentPartnerPubKeys |> List.remove_assoc(pubKey),
      partnerRemovals: [(id, processId), ...state.partnerRemovals],
    };
  | AccountCreationAccepted({data}) => {
      ...state,
      currentCustodians: [(data.accountIdx, []), ...state.currentCustodians],
    }
  | PayoutAccepted(_) => state
  | CustodianAccepted({data: {partnerId, accountIdx}}) =>
    let userChains =
      try (state.custodianKeyChains |> List.assoc(partnerId)) {
      | Not_found => []
      };
    let accountChains =
      try (userChains |> List.assoc(accountIdx)) {
      | Not_found => []
      };
    {
      ...state,
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
  | CustodianRemovalAccepted({processId, data: {custodianId, accountIdx}}) => {
      ...state,
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
  | TransactionConfirmed(_)
  | IncomeAddressExposed(_)
  | PayoutSigned(_)
  | PayoutAborted(_)
  | PayoutFinalized(_)
  | PayoutBroadcast(_)
  | PayoutBroadcastDuplicate(_)
  | PayoutBroadcastFailed(_)
  | PartnerDenied(_)
  | PartnerRemovalDenied(_)
  | CustodianDenied(_)
  | CustodianRemovalDenied(_)
  | PayoutDenied(_) => state
  };
};

type result =
  | Ok
  | Ignore
  | InvalidIssuer
  | UnknownProcessId
  | NotEligible
  | AlreadyVoted
  | PolicyNotFulfilled
  | PrematureDenial
  | DependencyNotMet
  | BadData(string);

let resultToString =
  fun
  | Ok => "Ok"
  | Ignore => "Ignore"
  | InvalidIssuer => "InvalidIssuer"
  | UnknownProcessId => "UnknownProcessId"
  | NotEligible => "NotEligible"
  | AlreadyVoted => "AlreadyVoted"
  | PolicyNotFulfilled => "PolicyNotFulfilled"
  | PrematureDenial => "PrematureDenial"
  | DependencyNotMet => "DependencyNotMet"
  | BadData(description) => "BadData('" ++ description ++ "')";

let processExists = (processId, {processValidator}) =>
  processValidator.exists(processId) ? Ok : UnknownProcessId;

let isEligible = (processId, voterId, {processValidator}) =>
  processValidator.isEligible(processId, voterId) ? Ok : NotEligible;

let hasYetToVote = (processId, voterId, {processValidator}) =>
  processValidator.didVote(processId, voterId) ? AlreadyVoted : Ok;

let policyFulfilled = (processId, {processValidator}) =>
  processValidator.policyFulfilled(processId) ? Ok : PolicyNotFulfilled;

let policyCanNotBeFulfilled = (processId, {processValidator}) =>
  processValidator.canPolicyBeFulfilled(processId) ? PrematureDenial : Ok;

let ensureDependencies =
    (
      ~proposals=ProcessId.emptySet,
      ~completions=ProcessId.emptySet,
      {processValidator},
    ) =>
  proposals
  |. Belt.Set.every(processValidator.exists)
  && completions
  |. Belt.Set.every(processValidator.completed) ?
    Ok : DependencyNotMet;

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
      _processName,
      _dataList: list((processId, (userId, 'a))),
      {proposerId, data, dependsOnProposals, dependsOnCompletions}:
        EventTypes.proposal('a),
      state,
      issuerId,
    ) =>
  if (UserId.neq(issuerId, proposerId)) {
    InvalidIssuer;
  } else {
    state
    |> test(
         ensureDependencies(
           ~proposals=dependsOnProposals,
           ~completions=dependsOnCompletions,
         ),
       )
    |> andThen(validateData(data))
    |> returnResult;
  };

let validateRejection =
    ({processId, rejectorId}: EventTypes.rejection, state, issuerId) =>
  UserId.neq(issuerId, rejectorId) ?
    InvalidIssuer :
    state
    |> test(processExists(processId))
    |> andThen(isEligible(processId, rejectorId))
    |> andThen(hasYetToVote(processId, rejectorId))
    |> returnResult;

let validateEndorsement =
    ({processId, supporterId}: EventTypes.endorsement, state, issuerId) =>
  UserId.neq(issuerId, supporterId) ?
    InvalidIssuer :
    state
    |> test(processExists(processId))
    |> andThen(isEligible(processId, supporterId))
    |> andThen(hasYetToVote(processId, supporterId))
    |> returnResult;

let validateAcceptance =
    (
      {processId, data, dependsOnCompletions}: EventTypes.acceptance('a),
      dataList: list((processId, (userId, 'a))),
      eq: ('a, 'a) => bool,
      state,
      _issuerId,
    ) =>
  if (eq(data, dataList |> List.assoc(processId) |> snd) == false) {
    BadData("Data doesn't match proposal");
  } else {
    state
    |> test(processExists(processId))
    |> andThen(policyFulfilled(processId))
    |> andThen(ensureDependencies(~completions=dependsOnCompletions))
    |> returnResult;
  };

let validateDenial = ({processId}: EventTypes.denial, state, _issuerId) =>
  state
  |> test(processExists(processId))
  |> andThen(policyCanNotBeFulfilled(processId))
  |> returnResult;

let validatePartnerData =
    (
      {id, lastPartnerRemovalProcess}: Partner.Data.t,
      {partnerRemovals, currentPartners},
    ) =>
  if (currentPartners |. Belt.Set.has(id)) {
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
  if (currentPartners |. Belt.Set.has(id) == false) {
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
      {processValidator, custodianData, custodianKeyChains} as state,
      issuerId,
    ) =>
  if (UserId.neq(issuerId, custodianId)) {
    InvalidIssuer;
  } else {
    let accountIdx = keyChain |> CustodianKeyChain.accountIdx;
    state
    |> test(accountExists(accountIdx))
    |> andThen(_ =>
         if (custodianData
             |> List.mem_assoc(custodianApprovalProcess) == false
             || processValidator.completed(custodianApprovalProcess) == false) {
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
         }
       )
    |> returnResult;
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
      {address: {coordinates, displayAddress}}: IncomeAddressExposed.t,
      {accountKeyChains},
      _issuerId,
    ) =>
  try (
    {
      let generatedAddress = accountKeyChains |> Address.find(coordinates);
      if (displayAddress == generatedAddress.displayAddress) {
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
  | PartnerDenied(denial) => validateDenial(denial)
  | PartnerRemovalDenied(denial) => validateDenial(denial)
  | CustodianDenied(denial) => validateDenial(denial)
  | CustodianRemovalDenied(denial) => validateDenial(denial)
  | PayoutDenied(denial) => validateDenial(denial)
  | CustodianKeyChainUpdated(update) =>
    validateCustodianKeyChainUpdated(update)
  | AccountKeyChainIdentified(update) =>
    validateAccountKeyChainIdentified(update)
  | AccountKeyChainActivated(update) =>
    validateAccountKeyChainActivated(update)
  | IncomeAddressExposed(event) => validateIncomeAddressExposed(event)
  | IncomeDetected(_) => ((_state, _pubKey) => Ok)
  | TransactionConfirmed(_) => ((_state, _pubKey) => Ok)
  | PayoutSigned(_) => ((_state, _pubKey) => Ok)
  | PayoutAborted(_) => ((_state, _pubKey) => Ok)
  | PayoutFinalized(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcast(_) => ((_state, _pubKey) => Ok)
  | PayoutBroadcastDuplicate(_) => ((_state, _pubKey) => Ignore)
  | PayoutBroadcastFailed(_) => ((_state, _pubKey) => Ok);

let validate =
    (
      ~partnerId as originId=None,
      {knownItems} as state,
      {hash, event, issuerPubKey}: EventLog.item,
    ) =>
  if (knownItems |. ItemsSet.has(hash)) {
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
          && Some(issuerPubKey) == state.creatorData.pubKey
          && state.partnerData
          |> List.length == 0 =>
      Ok
    | (PartnerEndorsed(event), false, false)
        when
          event.supporterId == state.creatorData.id
          && state.knownItems
          |> ItemsSet.size == 2 =>
      Ok
    | (PartnerPubKeyAdded({partnerId}), false, false) =>
      switch (originId) {
      | Some(originId) => UserId.eq(originId, partnerId) ? Ok : InvalidIssuer
      | None => InvalidIssuer
      }

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
