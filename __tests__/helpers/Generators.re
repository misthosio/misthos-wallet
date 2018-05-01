open PrimitiveTypes;

open WalletTypes;

module AppEvent = Event;

let userSession = id : Session.Data.t => {
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

let twoUserSessions = () => (
  userSession("user1" |> UserId.fromString),
  userSession("user2" |> UserId.fromString),
);

let threeUserSessions = () => (
  userSession("user1" |> UserId.fromString),
  userSession("user2" |> UserId.fromString),
  userSession("user3" |> UserId.fromString),
);

module Event = {
  let createVenture = (session: Session.Data.t) =>
    AppEvent.VentureCreated.make(
      ~ventureName=UserId.toString(session.userId) ++ "-testventure",
      ~creatorId=session.userId,
      ~creatorPubKey=session.issuerKeyPair |> Utils.publicKeyFromKeyPair,
      ~metaPolicy=Policy.unanimous,
      ~network=session.network,
    );
  let partnerProposed =
      (
        ~policy=Policy.unanimous,
        supporterSession: Session.Data.t,
        prospectSession: Session.Data.t,
      ) =>
    AppEvent.makePartnerProposed(
      ~supporterId=supporterSession.userId,
      ~prospectId=prospectSession.userId,
      ~prospectPubKey=
        prospectSession.issuerKeyPair |> Utils.publicKeyFromKeyPair,
      ~policy,
    )
    |> AppEvent.getPartnerProposedExn;
  let partnerEndorsed =
      (supporter: Session.Data.t, {processId}: AppEvent.Partner.Proposed.t) =>
    AppEvent.makePartnerEndorsed(~processId, ~supporterId=supporter.userId)
    |> AppEvent.getPartnerEndorsedExn;
  let partnerAccepted = AppEvent.Partner.Accepted.fromProposal;
  let partnerRemovalProposed =
      (supporterSession: Session.Data.t, toBeRemoved: Session.Data.t) =>
    AppEvent.makePartnerRemovalProposed(
      ~supporterId=supporterSession.userId,
      ~partnerId=toBeRemoved.userId,
      ~policy=Policy.unanimousMinusOne,
    )
    |> AppEvent.getPartnerRemovalProposedExn;
  let partnerRemovalEndorsed =
      (
        supporter: Session.Data.t,
        {processId}: AppEvent.Partner.Removal.Proposed.t,
      ) =>
    AppEvent.makePartnerRemovalEndorsed(
      ~processId,
      ~supporterId=supporter.userId,
    )
    |> AppEvent.getPartnerRemovalEndorsedExn;
  let partnerRemovalAccepted = AppEvent.Partner.Removal.Accepted.fromProposal;
  let accountCreationProposed = ({userId}: Session.Data.t) =>
    AppEvent.makeAccountCreationProposed(
      ~supporterId=userId,
      ~name="test",
      ~accountIdx=AccountIndex.default,
      ~policy=Policy.unanimous,
    )
    |> AppEvent.getAccountCreationProposedExn;
  let accountCreationAccepted = AppEvent.AccountCreation.Accepted.fromProposal;
  let custodianProposed =
      (
        {userId}: Session.Data.t,
        partnerProposal: AppEvent.Partner.Proposed.t,
      ) =>
    Event.makeCustodianProposed(
      ~partnerApprovalProcess=partnerProposal.processId,
      ~supporterId=userId,
      ~partnerId=partnerProposal.data.id,
      ~accountIdx=AccountIndex.default,
      ~policy=Policy.unanimous,
    )
    |> Event.getCustodianProposedExn;
  let custodianEndorsed =
      (supporter: Session.Data.t, {processId}: AppEvent.Custodian.Proposed.t) =>
    AppEvent.makeCustodianEndorsed(~processId, ~supporterId=supporter.userId)
    |> AppEvent.getCustodianEndorsedExn;
  let custodianAccepted = AppEvent.Custodian.Accepted.fromProposal;
};

module Log = {
  type t = {
    systemIssuer: Bitcoin.ECPair.t,
    lastItem: EventLog.item,
    log: EventLog.t,
  };
  let reduce = (f, s, {log}) => EventLog.reduce(f, s, log);
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
  let createVenture = (session: Session.Data.t) => {
    let ventureCreated = Event.createVenture(session);
    let (lastItem, log) =
      EventLog.make()
      |> EventLog.append(
           session.issuerKeyPair,
           VentureCreated(ventureCreated),
         );
    {systemIssuer: ventureCreated.systemIssuer, lastItem, log};
  };
  let withPartnerProposed =
      (
        ~issuer=?,
        ~policy=Policy.unanimous,
        ~supporter: Session.Data.t,
        ~prospect,
      ) => {
    let issuer =
      switch (issuer) {
      | None => supporter.issuerKeyPair
      | Some(key) => key
      };
    appendEvent(
      issuer,
      PartnerProposed(Event.partnerProposed(~policy, supporter, prospect)),
    );
  };
  let withPartnerEndorsed = (supporter: Session.Data.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      PartnerEndorsed(Event.partnerEndorsed(supporter, proposal)),
    );
  let withPartnerAccepted = proposal =>
    appendSystemEvent(PartnerAccepted(Event.partnerAccepted(proposal)));
  let withPartner = (user, ~supporters, log) =>
    switch (supporters) {
    | [first, ...rest] =>
      let log = log |> withPartnerProposed(~supporter=first, ~prospect=user);
      let proposal = log |> lastEvent |> AppEvent.getPartnerProposedExn;
      rest
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
  let withPartnerRemovalProposed = (~supporter: Session.Data.t, ~toBeRemoved) =>
    appendEvent(
      supporter.issuerKeyPair,
      PartnerRemovalProposed(
        Event.partnerRemovalProposed(supporter, toBeRemoved),
      ),
    );
  let withPartnerRemovalEndorsed = (supporter: Session.Data.t, proposal) =>
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
    | [first, ...rest] =>
      let log =
        log |> withPartnerRemovalProposed(~supporter=first, ~toBeRemoved=user);
      let proposal = log |> lastEvent |> AppEvent.getPartnerRemovalProposedExn;
      rest
      |> List.fold_left(
           (log, supporter) =>
             log |> withPartnerRemovalEndorsed(supporter, proposal),
           log,
         )
      |> withPartnerRemovalAccepted(proposal);
    | _ => %assert
           "withPartner"
    };
  let withAccountCreationProposed = (~supporter: Session.Data.t) =>
    appendEvent(
      supporter.issuerKeyPair,
      AccountCreationProposed(Event.accountCreationProposed(supporter)),
    );
  let withAccountCreationAccepted = proposal =>
    appendSystemEvent(
      AccountCreationAccepted(Event.accountCreationAccepted(proposal)),
    );
  let withCustodianProposed =
      (~supporter: Session.Data.t, ~custodian: Session.Data.t, {log} as l) => {
    let partnerProposed =
      log
      |> EventLog.reduce(
           (partnerProposal, {event}) =>
             switch (partnerProposal, event) {
             | (Some(p), _) => Some(p)
             | (_, PartnerProposed(proposal))
                 when UserId.eq(proposal.data.id, custodian.userId) =>
               Some(proposal)
             | _ => partnerProposal
             },
           None,
         );
    switch (partnerProposed) {
    | Some(proposal) =>
      appendEvent(
        supporter.issuerKeyPair,
        CustodianProposed(Event.custodianProposed(supporter, proposal)),
        l,
      )
    | None => raise(Not_found)
    };
  };
  let withCustodianEndorsed = (supporter: Session.Data.t, proposal) =>
    appendEvent(
      supporter.issuerKeyPair,
      CustodianEndorsed(Event.custodianEndorsed(supporter, proposal)),
    );
  let withCustodianAccepted = proposal =>
    appendSystemEvent(CustodianAccepted(Event.custodianAccepted(proposal)));
};
