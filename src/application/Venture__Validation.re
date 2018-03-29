open PrimitiveTypes;

open WalletTypes;

open Event;

type approvalProcess = {
  supporterIds: list(userId),
  policy: Policy.t
};

type state = {
  ventureName: string,
  systemPubKey: string,
  metaPolicy: Policy.t,
  partnerIds: list(userId),
  partnerAddresses: list(string),
  partnerPubKeys: list((string, userId)),
  partnerData: list((processId, Partner.Data.t)),
  custodianData: list((processId, Custodian.Data.t)),
  accountCreationData: list((processId, AccountCreation.Data.t)),
  processes: list((processId, approvalProcess)),
  completedProcesses: list(processId),
  policies: list((string, Policy.t)),
  creatorData: Partner.Data.t,
  custodianKeyChains:
    list((userId, list((accountIdx, list(CustodianKeyChain.public))))),
  accountKeyChains:
    list((accountIdx, list((accountKeyChainIdx, AccountKeyChain.t))))
};

let makeState = () => {
  ventureName: "",
  systemPubKey: "",
  partnerIds: [],
  partnerAddresses: [],
  partnerPubKeys: [],
  metaPolicy: Policy.absolute,
  partnerData: [],
  custodianData: [],
  accountCreationData: [],
  processes: [],
  completedProcesses: [],
  policies: [],
  creatorData: {
    id: UserId.fromString(""),
    pubKey: ""
  },
  custodianKeyChains: [],
  accountKeyChains: []
};

let addProcess =
    (
      {processId, policy, supporterId}: EventTypes.proposal('a),
      {processes} as state
    ) => {
  ...state,
  processes: [(processId, {policy, supporterIds: [supporterId]}), ...processes]
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
             {...process, supporterIds: [supporterId, ...process.supporterIds]}
           ) :
           (pId, process)
       )
};

let completeProcess =
    ({processId}: EventTypes.acceptance('a), {completedProcesses} as state) => {
  ...state,
  completedProcesses: [processId, ...completedProcesses]
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({
      ventureName,
      metaPolicy,
      systemIssuer,
      creatorId,
      creatorPubKey
    }) => {
      ...state,
      ventureName,
      systemPubKey: systemIssuer |> Utils.publicKeyFromKeyPair,
      metaPolicy,
      policies:
        [
          Partner.processName,
          AccountCreation.processName,
          Custodian.processName
        ]
        |> List.map(n => (n, metaPolicy)),
      creatorData: Partner.Data.{id: creatorId, pubKey: creatorPubKey}
    }
  | PartnerProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      partnerData: [(processId, data), ...state.partnerData]
    }
  | CustodianProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      custodianData: [(processId, data), ...state.custodianData]
    }
  | AccountCreationProposed({processId, data} as proposal) => {
      ...addProcess(proposal, state),
      accountCreationData: [(processId, data), ...state.accountCreationData]
    }
  | PartnerEndorsed(endorsement) => endorseProcess(endorsement, state)
  | CustodianEndorsed(endorsement) => endorseProcess(endorsement, state)
  | AccountCreationEndorsed(endorsement) => endorseProcess(endorsement, state)
  | PartnerAccepted({data}) => {
      ...state,
      partnerIds: [data.id, ...state.partnerIds],
      partnerAddresses: [
        Utils.addressFromPublicKey(data.pubKey),
        ...state.partnerAddresses
      ],
      partnerPubKeys: [(data.pubKey, data.id), ...state.partnerPubKeys]
    }
  | AccountCreationAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      accountKeyChains: [(data.accountIndex, []), ...state.accountKeyChains]
    }
  | CustodianAccepted({data} as acceptance) => {
      ...completeProcess(acceptance, state),
      custodianKeyChains: [
        (data.partnerId, [(data.accountIndex, [])]),
        ...state.custodianKeyChains
      ]
    }
  | CustodianKeyChainUpdated({partnerId, keyChain}) =>
    let userChains =
      try (state.custodianKeyChains |> List.assoc(partnerId)) {
      | Not_found => []
      };
    let accountChains =
      try (userChains |> List.assoc(CustodianKeyChain.accountIndex(keyChain))) {
      | Not_found => []
      };
    {
      ...state,
      custodianKeyChains: [
        (
          partnerId,
          [
            (
              CustodianKeyChain.accountIndex(keyChain),
              [keyChain, ...accountChains]
            )
          ]
        ),
        ...state.custodianKeyChains |> List.remove_assoc(partnerId)
      ]
    };
  | AccountKeyChainUpdated({accountIndex, keyChainIndex, keyChain}) =>
    let accountChains =
      try (state.accountKeyChains |> List.assoc(accountIndex)) {
      | Not_found => []
      };
    {
      ...state,
      accountKeyChains: [
        (accountIndex, [(keyChainIndex, keyChain), ...accountChains])
      ]
    };
  | IncomeAddressExposed(_) => state
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

let defaultDataValidator = (_, _) => Ok;

let validateProposal =
    (
      ~validateData: ('a, state) => result=defaultDataValidator,
      processName,
      {policy, supporterId, data, dependsOn}: EventTypes.proposal('a),
      {policies, partnerPubKeys, processes} as state,
      issuerPubKey
    ) =>
  if (Policy.neq(policy, policies |> List.assoc(processName))) {
    PolicyMissmatch;
  } else if (UserId.neq(
               partnerPubKeys |> List.assoc(issuerPubKey),
               supporterId
             )) {
    InvalidIssuer;
  } else {
    switch dependsOn {
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
      issuerPubKey
    ) =>
  try {
    let {supporterIds} = processes |> List.assoc(processId);
    if (UserId.neq(partnerPubKeys |> List.assoc(issuerPubKey), supporterId)) {
      InvalidIssuer;
    } else if (supporterIds |> List.mem(supporterId)) {
      DuplicateEndorsement;
    } else {
      Ok;
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateAcceptance =
    (
      {processId, data, dependsOn}: EventTypes.acceptance('a),
      dataList: list((processId, 'a)),
      {processes, partnerIds, completedProcesses},
      _issuerPubKey
    ) =>
  try {
    let {policy, supporterIds} = processes |> List.assoc(processId);
    if (data != (dataList |> List.assoc(processId))) {
      BadData("Data doesn't match proposal");
    } else if (Policy.fulfilled(
                 ~eligable=partnerIds,
                 ~endorsed=supporterIds,
                 policy
               )
               == false) {
      PolicyNotFulfilled;
    } else {
      switch dependsOn {
      | None => Ok
      | Some(processId) =>
        completedProcesses |> List.mem(processId) ? Ok : DependencyNotMet
      };
    };
  } {
  | Not_found => UnknownProcessId
  };

let validateCustodianData = (data: Custodian.Data.t, {partnerIds}) =>
  partnerIds |> List.mem(data.partnerId) ?
    Ok :
    BadData(
      "Partner with Id '"
      ++ UserId.toString(data.partnerId)
      ++ "' doesn't exist"
    );

let validateAccountCreationData =
    ({accountIndex}: AccountCreation.Data.t, {accountCreationData}) =>
  accountIndex |> AccountIndex.toInt == (accountCreationData |> List.length) ?
    Ok : BadData("Bad Account Index");

let validateCustodianKeyChainUpdated =
    (
      {partnerId, keyChain}: CustodianKeyChainUpdated.t,
      {partnerPubKeys, custodianData, completedProcesses, custodianKeyChains},
      issuerPubKey
    ) =>
  if (UserId.neq(partnerPubKeys |> List.assoc(issuerPubKey), partnerId)) {
    InvalidIssuer;
  } else {
    try {
      let (process, _data) =
        custodianData
        |> List.find(((_pId, data: Custodian.Data.t)) =>
             data.partnerId == partnerId
             && data.accountIndex == CustodianKeyChain.accountIndex(keyChain)
           );
      if (completedProcesses |> List.mem(process)) {
        if (custodianKeyChains
            |> List.assoc(partnerId)
            |> List.assoc(CustodianKeyChain.accountIndex(keyChain))
            |>
            List.length != (
                             CustodianKeyChain.keyChainIndex(keyChain)
                             |> CustodianKeyChainIndex.toInt
                           )) {
          BadData("Bad KeyChainIndex");
        } else {
          Ok;
        };
      } else {
        BadData("Custodian isn't accepted yet");
      };
    } {
    | Not_found => BadData("Custodian doesn't exist")
    };
  };

let validateAccountKeyChainUpdated =
    (
      {accountIndex, keyChainIndex, keyChain}: AccountKeyChainUpdated.t,
      {
        accountCreationData,
        completedProcesses,
        custodianKeyChains,
        accountKeyChains
      },
      _issuerPubKey
    ) =>
  try {
    let (pId, _) =
      accountCreationData
      |> List.find(((_, data: AccountCreation.Data.t)) =>
           data.accountIndex == accountIndex
         );
    if (completedProcesses |> List.mem(pId)) {
      if (accountKeyChains
          |> List.assoc(accountIndex)
          |> List.length != (keyChainIndex |> AccountKeyChainIndex.toInt)) {
        BadData("Bad KeyChainIndex");
      } else {
        keyChain
        |> AccountKeyChain.custodianKeyChains
        |> List.map(((partnerId, keyChain)) =>
             try {
               let latestKeyChain =
                 custodianKeyChains
                 /* list((userId, list((int, list(CustodianKeyChain.public))))), */
                 |> List.assoc(partnerId)
                 |> List.assoc(accountIndex)
                 |> List.sort((keysA, keysB) =>
                      compare(
                        keysA |> CustodianKeyChain.keyChainIndex,
                        keysB |> CustodianKeyChain.keyChainIndex
                      )
                    )
                 |> List.rev
                 |> List.hd;
               keyChain == latestKeyChain ?
                 Ok : BadData("Bad CustodianKeyChain");
             } {
             | Not_found => BadData("Bad CustodianKeyChain")
             }
           )
        |> List.fold_left((result, test) => test == Ok ? result : test, Ok);
      };
    } else {
      BadData("Account doesn't exist");
    };
  } {
  | Not_found => BadData("Account doesn't exist")
  };

let validateIncomeAddressExposed =
    (
      {coordinates, address}: IncomeAddressExposed.t,
      {accountKeyChains},
      _issuerPubKey
    ) =>
  try {
    let generatedAddress =
      accountKeyChains |> AccountKeyChain.find(coordinates);
    if (address == generatedAddress.address) {
      Ok;
    } else {
      BadData("Unknown Address");
    };
  } {
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
      proposal
    )
  | AccountCreationProposed(proposal) =>
    validateProposal(
      ~validateData=validateAccountCreationData,
      AccountCreation.processName,
      proposal
    )
  | PartnerEndorsed(endorsement) => validateEndorsement(endorsement)
  | CustodianEndorsed(endorsement) => validateEndorsement(endorsement)
  | AccountCreationEndorsed(endorsement) => validateEndorsement(endorsement)
  | PartnerAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.partnerData, state)
    )
  | CustodianAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.custodianData, state)
    )
  | AccountCreationAccepted(acceptance) => (
      state => validateAcceptance(acceptance, state.accountCreationData, state)
    )
  | CustodianKeyChainUpdated(update) =>
    validateCustodianKeyChainUpdated(update)
  | AccountKeyChainUpdated(update) => validateAccountKeyChainUpdated(update)
  | IncomeAddressExposed(event) => validateIncomeAddressExposed(event);

let validate = (state, {event, issuerPubKey}: EventLog.item) =>
  switch (
    event,
    Event.isSystemEvent(event),
    state.partnerPubKeys |> List.mem_assoc(issuerPubKey)
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
