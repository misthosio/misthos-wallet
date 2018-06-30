open PrimitiveTypes;

open WalletTypes;

module AppEvent = Event;

let userSession = id : SessionData.t => {
  let appPrivateKey = Crypto.randomBytes(32) |> Utils.bufToHex;
  let issuerKeyPair =
    Utils.keyPairFromPrivateKey(
      Network.bitcoinNetwork(Regtest),
      appPrivateKey,
    );
  let appPubKey = issuerKeyPair |> Utils.publicKeyFromKeyPair;
  {
    userId: id,
    appPrivateKey,
    issuerKeyPair,
    storagePrefix:
      UserInfo.storagePrefix(
        ~appPubKey=issuerKeyPair |> Utils.publicKeyFromKeyPair,
      ),
    masterKeyChain:
      Bitcoin.HDNode.make(
        issuerKeyPair,
        appPubKey |. String.sub(0, 64) |> Utils.bufFromHex,
      ),
    network: Regtest,
  };
};

let withUserSessions = n =>
  Belt.List.makeBy(n, i =>
    userSession("user" ++ string_of_int(i) |> UserId.fromString)
  )
  |> Array.of_list;

let twoUserSessionsFromArray = sessions => (sessions[0], sessions[1]);

let threeUserSessionsFromArray = sessions => (
  sessions[0],
  sessions[1],
  sessions[2],
);

let fourUserSessionsFromArray = sessions => (
  sessions[0],
  sessions[1],
  sessions[2],
  sessions[3],
);

let fiveUserSessionsFromArray = sessions => (
  sessions[0],
  sessions[1],
  sessions[2],
  sessions[3],
  sessions[4],
);

let twoUserSessions = () => (
  userSession("user1" |> UserId.fromString),
  userSession("user2" |> UserId.fromString),
);

let threeUserSessions = () => (
  userSession("user1" |> UserId.fromString),
  userSession("user2" |> UserId.fromString),
  userSession("user3" |> UserId.fromString),
);

let fourUserSessions = () => (
  userSession("user1" |> UserId.fromString),
  userSession("user2" |> UserId.fromString),
  userSession("user3" |> UserId.fromString),
  userSession("user4" |> UserId.fromString),
);

let custodianKeyChain =
    (
      ~accountIdx=AccountIndex.default,
      ~ventureId,
      ~keyChainIdx,
      {masterKeyChain}: SessionData.t,
    ) =>
  CustodianKeyChain.make(
    ~ventureId,
    ~accountIdx,
    ~keyChainIdx=CustodianKeyChainIndex.fromInt(keyChainIdx),
    ~masterKeyChain,
  )
  |> CustodianKeyChain.toPublicKeyChain;

let accountKeyChainFrom = AccountKeyChain.make(AccountIndex.default);

let accountKeyChain =
    (~ventureId=VentureId.fromString("test"), ~keyChainIdx=0, users) =>
  users
  |> List.map((user: SessionData.t) =>
       (user.userId, custodianKeyChain(~ventureId, ~keyChainIdx, user))
     )
  |> accountKeyChainFrom;

module Event = {
  let createVenture = (session: SessionData.t) =>
    AppEvent.VentureCreated.make(
      ~ventureName=UserId.toString(session.userId) ++ "-testventure",
      ~creatorId=session.userId,
      ~creatorPubKey=session.issuerKeyPair |> Utils.publicKeyFromKeyPair,
      ~metaPolicy=Policy.unanimous,
      ~network=session.network,
    );
  let partnerProposed =
      (
        ~withPubKey=true,
        ~eligibleWhenProposing,
        ~policy=Policy.unanimous,
        ~lastRemovalAccepted,
        proposerSession: SessionData.t,
        prospectSession: SessionData.t,
      ) =>
    (
      if (withPubKey) {
        AppEvent.makePartnerProposed(
          ~prospectPubKey=
            prospectSession.issuerKeyPair |> Utils.publicKeyFromKeyPair,
          ~eligibleWhenProposing,
          ~proposerId=proposerSession.userId,
          ~prospectId=prospectSession.userId,
          ~policy,
          ~lastRemovalAccepted,
          (),
        );
      } else {
        AppEvent.makePartnerProposed(
          ~proposerId=proposerSession.userId,
          ~prospectId=prospectSession.userId,
          ~eligibleWhenProposing,
          ~policy,
          ~lastRemovalAccepted,
          (),
        );
      }
    )
    |> AppEvent.getPartnerProposedExn;
  let partnerEndorsed =
      (supporter: SessionData.t, {processId}: AppEvent.Partner.Proposed.t) =>
    AppEvent.makePartnerEndorsed(~processId, ~supporterId=supporter.userId)
    |> AppEvent.getPartnerEndorsedExn;
  let partnerRejected =
      (rejector: SessionData.t, {processId}: AppEvent.Partner.Proposed.t) =>
    AppEvent.makePartnerRejected(~processId, ~rejectorId=rejector.userId)
    |> AppEvent.getPartnerRejectedExn;
  let partnerAccepted = AppEvent.Partner.Accepted.fromProposal;
  let partnerDenied = AppEvent.Partner.Denied.fromProposal;
  let partnerPubKeyAdded = (partner: SessionData.t) =>
    AppEvent.Partner.PubKeyAdded.make(
      ~partnerId=partner.userId,
      ~pubKey=partner.issuerKeyPair |> Utils.publicKeyFromKeyPair,
    );
  let partnerRemovalProposed =
      (
        ~eligibleWhenProposing,
        ~lastPartnerAccepted,
        proposerSession: SessionData.t,
      ) =>
    AppEvent.makePartnerRemovalProposed(
      ~eligibleWhenProposing,
      ~lastPartnerAccepted,
      ~proposerId=proposerSession.userId,
      ~policy=Policy.unanimousMinusOne,
    )
    |> AppEvent.getPartnerRemovalProposedExn;
  let partnerRemovalEndorsed =
      (
        supporter: SessionData.t,
        {processId}: AppEvent.Partner.Removal.Proposed.t,
      ) =>
    AppEvent.makePartnerRemovalEndorsed(
      ~processId,
      ~supporterId=supporter.userId,
    )
    |> AppEvent.getPartnerRemovalEndorsedExn;
  let partnerRemovalAccepted = AppEvent.Partner.Removal.Accepted.fromProposal;
  let accountCreationProposed =
      (~eligibleWhenProposing, {userId}: SessionData.t) =>
    AppEvent.makeAccountCreationProposed(
      ~eligibleWhenProposing,
      ~proposerId=userId,
      ~name="test",
      ~accountIdx=AccountIndex.default,
      ~policy=Policy.unanimous,
    )
    |> AppEvent.getAccountCreationProposedExn;
  let accountCreationEndorsed =
      (
        supporter: SessionData.t,
        {processId}: AppEvent.AccountCreation.Proposed.t,
      ) =>
    AppEvent.makeAccountCreationEndorsed(
      ~processId,
      ~supporterId=supporter.userId,
    )
    |> AppEvent.getAccountCreationEndorsedExn;
  let accountCreationAccepted = AppEvent.AccountCreation.Accepted.fromProposal;
  let custodianProposed =
      (
        ~eligibleWhenProposing,
        ~lastCustodianRemovalAccepted,
        {userId}: SessionData.t,
        partnerProposal: AppEvent.Partner.Proposed.t,
      ) =>
    Event.makeCustodianProposed(
      ~eligibleWhenProposing,
      ~lastCustodianRemovalAccepted,
      ~partnerProposed=partnerProposal,
      ~proposerId=userId,
      ~accountIdx=AccountIndex.default,
      ~policy=Policy.unanimous,
    )
    |> Event.getCustodianProposedExn;
  let custodianEndorsed =
      (supporter: SessionData.t, {processId}: AppEvent.Custodian.Proposed.t) =>
    AppEvent.makeCustodianEndorsed(~processId, ~supporterId=supporter.userId)
    |> AppEvent.getCustodianEndorsedExn;
  let custodianRejected =
      (rejector: SessionData.t, {processId}: AppEvent.Custodian.Proposed.t) =>
    AppEvent.makeCustodianRejected(~processId, ~rejectorId=rejector.userId)
    |> AppEvent.getCustodianRejectedExn;
  let custodianAccepted = AppEvent.Custodian.Accepted.fromProposal;
  let custodianDenied = AppEvent.Custodian.Denied.fromProposal;
  let custodianRemovalProposed =
      (
        ~eligibleWhenProposing,
        ~custodianAccepted,
        proposerSession: SessionData.t,
      ) =>
    AppEvent.makeCustodianRemovalProposed(
      ~eligibleWhenProposing,
      ~custodianAccepted,
      ~proposerId=proposerSession.userId,
      ~accountIdx=AccountIndex.default,
      ~policy=Policy.unanimousMinusOne,
    )
    |> AppEvent.getCustodianRemovalProposedExn;
  let custodianRemovalEndorsed =
      (
        supporter: SessionData.t,
        {processId}: AppEvent.Custodian.Removal.Proposed.t,
      ) =>
    AppEvent.makeCustodianRemovalEndorsed(
      ~processId,
      ~supporterId=supporter.userId,
    )
    |> AppEvent.getCustodianRemovalEndorsedExn;
  let custodianRemovalAccepted = AppEvent.Custodian.Removal.Accepted.fromProposal;
  let custodianKeyChainUpdated = AppEvent.CustodianKeyChainUpdated.make;
  let accountKeyChainIdentified = AppEvent.AccountKeyChainIdentified.make;
  let accountKeyChainActivated =
      (~sequence=0, ~custodian: SessionData.t, ~identifier) =>
    AppEvent.AccountKeyChainActivated.make(
      ~accountIdx=AccountIndex.default,
      ~custodianId=custodian.userId,
      ~identifier,
      ~sequence,
    );
  let incomeAddressExposed = AppEvent.IncomeAddressExposed.make;
  let incomeDetected = (~address, ~coordinates) =>
    AppEvent.IncomeDetected.make(
      ~address,
      ~coordinates,
      ~txOutputN=0,
      ~txId=Uuid.v4(),
      ~amount=BTC.fromSatoshis(10000000L),
    );
};

module Log = {
  type t = {
    ventureId,
    systemIssuer: Bitcoin.ECPair.t,
    lastItem: EventLog.item,
    log: EventLog.t,
  };
  let eligiblePartners = ({log}) =>
    log
    |> EventLog.reduce(
         (res, {event}: EventLog.item) =>
           switch (event) {
           | PartnerAccepted({data: {id}}) => [id, ...res]
           | PartnerRemovalAccepted({data: {id}}) =>
             res |> List.filter(UserId.neq(id))
           | _ => res
           },
         [],
       )
    |> Array.of_list
    |> Belt.Set.mergeMany(UserId.emptySet);
  let reduce = (f, s, {log}) => EventLog.reduce(f, s, log);
  let ventureId = ({ventureId}) => ventureId;
  let systemIssuer = ({systemIssuer}) => systemIssuer;
  let lastItem = ({lastItem}) => lastItem;
  let lastEvent = ({lastItem: {event}}) => event;
  let eventLog = ({log}) => log;
  let appendEvent = (issuer, event, {log} as l) => {
    let (lastItem, log) = log |> EventLog.append(issuer, event);
    {...l, lastItem, log};
  };
  let appendSystemEvent = (event, {systemIssuer} as log) =>
    appendEvent(systemIssuer, event, log);
  let fromEventLog = log => {
    let (ventureId, systemIssuer, lastItem) =
      log
      |> EventLog.reduce(
           (
             (ventureId, systemIssuer, _),
             {event} as lastItem: EventLog.item,
           ) =>
             switch (event) {
             | VentureCreated({ventureId, systemIssuer}) => (
                 Some(ventureId),
                 Some(systemIssuer),
                 Some(lastItem),
               )
             | _ => (ventureId, systemIssuer, Some(lastItem))
             },
           (None, None, None),
         );
    {
      ventureId: ventureId |> Js.Option.getExn,
      systemIssuer: systemIssuer |> Js.Option.getExn,
      lastItem: lastItem |> Js.Option.getExn,
      log,
    };
  };
  let make = (session: SessionData.t, ventureCreated) => {
    let (lastItem, log) =
      EventLog.make()
      |> EventLog.append(
           session.issuerKeyPair,
           VentureCreated(ventureCreated),
         );
    {
      ventureId: ventureCreated.ventureId,
      systemIssuer: ventureCreated.systemIssuer,
      lastItem,
      log,
    };
  };
  let createVenture = (session: SessionData.t) =>
    make(session, Event.createVenture(session));
  let withPartnerProposed =
      (
        ~withPubKey=true,
        ~withLastRemoval=true,
        ~issuer=?,
        ~policy=Policy.unanimous,
        ~proposer: SessionData.t,
        ~prospect: SessionData.t,
        {log} as l,
      ) => {
    let issuer =
      switch (issuer) {
      | None => proposer.issuerKeyPair
      | Some(key) => key
      };
    let lastRemovalAccepted =
      withLastRemoval ?
        log
        |> EventLog.reduce(
             (res, {event}) =>
               switch (event) {
               | PartnerRemovalAccepted({data: {id}} as event)
                   when UserId.eq(id, prospect.userId) =>
                 Some(event)
               | _ => res
               },
             None,
           ) :
        None;
    appendEvent(
      issuer,
      PartnerProposed(
        Event.partnerProposed(
          ~withPubKey,
          ~eligibleWhenProposing=eligiblePartners(l),
          ~policy,
          ~lastRemovalAccepted,
          proposer,
          prospect,
        ),
      ),
      l,
    );
  };
  let withPartnerEndorsed = (~issuer=?, supporter: SessionData.t, proposal) => {
    let issuer =
      switch (issuer) {
      | None => supporter.issuerKeyPair
      | Some(key) => key
      };
    appendEvent(
      issuer,
      PartnerEndorsed(Event.partnerEndorsed(supporter, proposal)),
    );
  };
  let withPartnerRejected = (~issuer=?, supporter: SessionData.t, proposal) => {
    let issuer =
      switch (issuer) {
      | None => supporter.issuerKeyPair
      | Some(key) => key
      };
    appendEvent(
      issuer,
      PartnerRejected(Event.partnerRejected(supporter, proposal)),
    );
  };
  let withPartnerAccepted = proposal =>
    appendSystemEvent(PartnerAccepted(Event.partnerAccepted(proposal)));
  let withPartnerDenied = proposal =>
    appendSystemEvent(PartnerDenied(Event.partnerDenied(proposal)));
  let withPartner = (~withPubKey=true, user, ~supporters, log) =>
    switch (supporters) {
    | [first, ..._rest] =>
      let log =
        log
        |> withPartnerProposed(~withPubKey, ~proposer=first, ~prospect=user);
      let proposal = log |> lastEvent |> AppEvent.getPartnerProposedExn;
      supporters
      |> List.fold_left(
           (log, supporter) =>
             log |> withPartnerEndorsed(supporter, proposal),
           log,
         )
      |> withPartnerAccepted(proposal);
    | _ => %assert
           "withPartner"
    };
  let withFirstPartner = user => withPartner(user, ~supporters=[user]);
  let withPartnerRemovalProposed =
      (~proposer: SessionData.t, ~toBeRemoved: SessionData.t, {log} as l) => {
    let lastPartnerAccepted =
      log
      |> EventLog.reduce(
           (res, {event}) =>
             switch (event) {
             | PartnerAccepted({data: {id}} as event)
                 when UserId.eq(id, toBeRemoved.userId) =>
               Some(event)
             | _ => res
             },
           None,
         )
      |> Js.Option.getExn;
    l
    |> appendEvent(
         proposer.issuerKeyPair,
         PartnerRemovalProposed(
           Event.partnerRemovalProposed(
             ~eligibleWhenProposing=eligiblePartners(l),
             ~lastPartnerAccepted,
             proposer,
           ),
         ),
       );
  };
  let withPartnerPubKeyAdded = (partner: SessionData.t) =>
    appendEvent(
      partner.issuerKeyPair,
      PartnerPubKeyAdded(Event.partnerPubKeyAdded(partner)),
    );
  let withPartnerRemovalEndorsed = (supporter: SessionData.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      PartnerRemovalEndorsed(
        Event.partnerRemovalEndorsed(supporter, proposal),
      ),
    );
  let withPartnerRemovalAccepted = proposal =>
    appendSystemEvent(
      PartnerRemovalAccepted(Event.partnerRemovalAccepted(proposal)),
    );
  let withPartnerRemoved = (user, ~supporters, log) =>
    switch (supporters) {
    | [first, ..._rest] =>
      let log =
        log |> withPartnerRemovalProposed(~proposer=first, ~toBeRemoved=user);
      let proposal = log |> lastEvent |> AppEvent.getPartnerRemovalProposedExn;
      supporters
      |> List.fold_left(
           (log, supporter) =>
             log |> withPartnerRemovalEndorsed(supporter, proposal),
           log,
         )
      |> withPartnerRemovalAccepted(proposal);
    | _ => %assert
           "withPartner"
    };
  let withAccountCreationProposed = (~proposer: SessionData.t) =>
    appendEvent(
      proposer.issuerKeyPair,
      AccountCreationProposed(
        Event.accountCreationProposed(
          ~eligibleWhenProposing=
            [|proposer.userId|] |> Belt.Set.mergeMany(UserId.emptySet),
          proposer,
        ),
      ),
    );
  let withAccountCreationEndorsed = (supporter: SessionData.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      AccountCreationEndorsed(
        Event.accountCreationEndorsed(supporter, proposal),
      ),
    );
  let withAccountCreationAccepted = proposal =>
    appendSystemEvent(
      AccountCreationAccepted(Event.accountCreationAccepted(proposal)),
    );
  let withAccount = (~supporter, log) => {
    let log = log |> withAccountCreationProposed(~proposer=supporter);
    let proposal = log |> lastEvent |> AppEvent.getAccountCreationProposedExn;
    log
    |> withAccountCreationEndorsed(supporter, proposal)
    |> withAccountCreationAccepted(proposal);
  };
  let withCustodianProposed =
      (~proposer: SessionData.t, ~custodian: SessionData.t, {log} as l) => {
    let (partnerProposed, lastCustodianRemovalAccepted) =
      log
      |> EventLog.reduce(
           ((partnerProposal, custodianRemoved), {event}) =>
             switch (event) {
             | PartnerProposed(proposal)
                 when UserId.eq(proposal.data.id, custodian.userId) => (
                 Some(proposal),
                 custodianRemoved,
               )
             | CustodianRemovalAccepted(removal)
                 when UserId.eq(removal.data.custodianId, custodian.userId) => (
                 partnerProposal,
                 Some(removal),
               )
             | _ => (partnerProposal, custodianRemoved)
             },
           (None, None),
         );
    appendEvent(
      proposer.issuerKeyPair,
      CustodianProposed(
        Event.custodianProposed(
          ~eligibleWhenProposing=eligiblePartners(l),
          ~lastCustodianRemovalAccepted,
          proposer,
          partnerProposed |> Js.Option.getExn,
        ),
      ),
      l,
    );
  };
  let withCustodianEndorsed = (supporter: SessionData.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      CustodianEndorsed(Event.custodianEndorsed(supporter, proposal)),
    );
  let withCustodianRejected = (rejector: SessionData.t, proposal) =>
    appendEvent(
      rejector.issuerKeyPair,
      CustodianRejected(Event.custodianRejected(rejector, proposal)),
    );
  let withCustodianAccepted = proposal =>
    appendSystemEvent(CustodianAccepted(Event.custodianAccepted(proposal)));
  let withCustodianDenied = proposal =>
    appendSystemEvent(CustodianDenied(Event.custodianDenied(proposal)));
  let withCustodian = (user, ~supporters, log) =>
    switch (supporters) {
    | [first, ..._rest] =>
      let log =
        log |> withCustodianProposed(~proposer=first, ~custodian=user);
      let proposal = log |> lastEvent |> AppEvent.getCustodianProposedExn;
      supporters
      |> List.fold_left(
           (log, supporter) =>
             log |> withCustodianEndorsed(supporter, proposal),
           log,
         )
      |> withCustodianAccepted(proposal);
    | _ => %assert
           "withPartner"
    };
  let withCustodianRemovalProposed =
      (~proposer: SessionData.t, ~toBeRemoved: SessionData.t, {log} as l) => {
    let custodianAccepted =
      log
      |> EventLog.reduce(
           (res, {event}) =>
             switch (event) {
             | CustodianAccepted({data: {partnerId}} as event)
                 when UserId.eq(partnerId, toBeRemoved.userId) =>
               Some(event)
             | _ => res
             },
           None,
         )
      |> Js.Option.getExn;
    l
    |> appendEvent(
         proposer.issuerKeyPair,
         CustodianRemovalProposed(
           Event.custodianRemovalProposed(
             ~eligibleWhenProposing=eligiblePartners(l),
             ~custodianAccepted,
             proposer,
           ),
         ),
       );
  };
  let withCustodianRemovalEndorsed = (supporter: SessionData.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      CustodianRemovalEndorsed(
        Event.custodianRemovalEndorsed(supporter, proposal),
      ),
    );
  let withCustodianRemovalAccepted = proposal =>
    appendSystemEvent(
      CustodianRemovalAccepted(Event.custodianRemovalAccepted(proposal)),
    );
  let withCustodianRemoved = (user, ~supporters, log) =>
    switch (supporters) {
    | [first, ..._rest] =>
      let log =
        log
        |> withCustodianRemovalProposed(~proposer=first, ~toBeRemoved=user);
      let proposal =
        log |> lastEvent |> AppEvent.getCustodianRemovalProposedExn;
      supporters
      |> List.fold_left(
           (log, supporter) =>
             log |> withCustodianRemovalEndorsed(supporter, proposal),
           log,
         )
      |> withCustodianRemovalAccepted(proposal);
    | _ => %assert
           "withCustodian"
    };
  let withCustodianKeyChain =
      (~keyChainIdx=0, ~issuer=?, custodian, {log, ventureId} as l) => {
    let custodianProcesses =
      log
      |> EventLog.reduce(
           (res, {event}) =>
             switch (event) {
             | CustodianAccepted({processId, data: {partnerId}}) => [
                 (partnerId, processId),
                 ...res,
               ]
             | _ => res
             },
           [],
         );
    let keyChain = custodianKeyChain(~ventureId, ~keyChainIdx, custodian);
    let issuerKeyPair =
      issuer
      |> Utils.mapOption((issuer: SessionData.t) => issuer.issuerKeyPair)
      |> Js.Option.getWithDefault(custodian.issuerKeyPair);
    l
    |> appendEvent(
         issuerKeyPair,
         CustodianKeyChainUpdated(
           Event.custodianKeyChainUpdated(
             ~custodianApprovalProcess=
               custodianProcesses |> List.assoc(custodian.userId),
             ~custodianId=custodian.userId,
             ~keyChain,
           ),
         ),
       );
  };
  let withAccountKeyChainIdentified = ({log} as l) => {
    let keyChains =
      log
      |> EventLog.reduce(
           (res, {event}) =>
             switch (event) {
             | CustodianKeyChainUpdated({custodianId, keyChain}) => [
                 (custodianId, keyChain),
                 ...res |> List.remove_assoc(custodianId),
               ]
             | CustodianRemovalAccepted({data: {custodianId}}) =>
               try (res |> List.remove_assoc(custodianId)) {
               | Not_found => res
               }
             | PartnerRemovalAccepted({data: {id}}) =>
               try (res |> List.remove_assoc(id)) {
               | Not_found => res
               }
             | _ => res
             },
           [],
         );
    let accountKeyChain = accountKeyChainFrom(keyChains);
    l
    |> appendSystemEvent(
         AccountKeyChainIdentified(
           Event.accountKeyChainIdentified(~keyChain=accountKeyChain),
         ),
       );
  };
  let withAccountKeyChainActivated =
      (~sequence=0, user: SessionData.t, {log} as l) => {
    let identifier =
      log
      |> EventLog.reduce(
           (res, {event}) =>
             switch (event) {
             | AccountKeyChainIdentified({keyChain: {identifier}}) => identifier
             | _ => res
             },
           "",
         );
    l
    |> appendEvent(
         user.issuerKeyPair,
         AccountKeyChainActivated(
           Event.accountKeyChainActivated(
             ~sequence,
             ~custodian=user,
             ~identifier,
           ),
         ),
       );
  };
  let withIncomeAddressExposed = (user: SessionData.t, {log} as l) => {
    let (keyChains, activations, exposed) =
      log
      |> EventLog.reduce(
           ((keyChains, activations, exposed), {event}: EventLog.item) =>
             switch (event) {
             | AccountKeyChainIdentified({
                 keyChain: {identifier} as keyChain,
               }) => (
                 [(identifier, keyChain), ...keyChains],
                 activations,
                 exposed,
               )
             | AccountKeyChainActivated({custodianId, identifier}) => (
                 keyChains,
                 [(custodianId, identifier), ...activations],
                 exposed,
               )
             | IncomeAddressExposed({address: {coordinates}}) => (
                 keyChains,
                 activations,
                 [coordinates, ...exposed],
               )
             | _ => (keyChains, activations, exposed)
             },
           ([], [], []),
         );
    let keyChain =
      activations |> List.assoc(user.userId) |. List.assoc(keyChains);
    let coordinates =
      Address.Coordinates.nextExternal(user.userId, exposed, keyChain);
    let address = keyChain |> Address.make(coordinates);
    l
    |> appendEvent(
         user.issuerKeyPair,
         IncomeAddressExposed(
           Event.incomeAddressExposed(~partnerId=user.userId, ~address),
         ),
       );
  };
  let withIncomeDetected = (~incomeAddress, {log} as l) => {
    let (incomeDetected, _) =
      log
      |> EventLog.reduce(
           ((res, counter), {event}) =>
             switch (counter, event) {
             | (counter, IncomeAddressExposed(_)) when counter > 0 => (
                 None,
                 counter - 1,
               )
             | (0, IncomeAddressExposed({address})) => (
                 Some(
                   Event.incomeDetected(
                     ~address=address.displayAddress,
                     ~coordinates=address.coordinates,
                   ),
                 ),
                 (-1),
               )
             | _ => (res, counter)
             },
           (None, incomeAddress),
         );
    l
    |> appendSystemEvent(IncomeDetected(incomeDetected |> Js.Option.getExn));
  };
};
