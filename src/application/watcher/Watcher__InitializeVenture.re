open Event;

open PrimitiveTypes;

open WalletTypes;

let defaultAccountName = "default";

type state =
  | ProposePartner
  | EndorsePartner(Partner.Proposed.t)
  | PartnerEndorsed(Partner.Proposed.t)
  | ProposeAccountCreation(Partner.Proposed.t)
  | EndorseAccountCreation(processId, Partner.Proposed.t)
  | AccountCreationEndorsed(processId, Partner.Proposed.t)
  | ProposeCustodian(Partner.Proposed.t)
  | EndorseCustodian(processId)
  | Complete;

let make =
    (
      {userId, issuerKeyPair}: SessionData.t,
      {
        creatorId,
        creatorPubKey,
        defaultAccountSettings,
        initialPolicies,
        metaPolicy,
      }: VentureCreated.t,
      log,
    ) => {
  let process = {
    val state =
      ref(UserId.eq(userId, creatorId) ? ProposePartner : Complete);
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (state^, event) {
          | (ProposePartner, PartnerProposed(event))
              when UserId.eq(event.data.id, creatorId) =>
            EndorsePartner(event)
          | (EndorsePartner(partnerProposedEvent), PartnerEndorsed(event))
              when
                ProcessId.eq(partnerProposedEvent.processId, event.processId) =>
            PartnerEndorsed(partnerProposedEvent)
          | (PartnerEndorsed(partnerProposedEvent), PartnerAccepted(event))
              when
                ProcessId.eq(partnerProposedEvent.processId, event.processId) =>
            ProposeAccountCreation(partnerProposedEvent)
          | (
              ProposeAccountCreation(partnerProposedEvent),
              AccountCreationProposed(event),
            )
              when
                AccountIndex.eq(event.data.accountIdx, AccountIndex.default) =>
            EndorseAccountCreation(event.processId, partnerProposedEvent)
          | (
              EndorseAccountCreation(processId, partnerProcess),
              AccountCreationEndorsed(event),
            )
              when ProcessId.eq(processId, event.processId) =>
            AccountCreationEndorsed(processId, partnerProcess)
          | (
              AccountCreationEndorsed(processId, partnerProcess),
              AccountCreationAccepted(event),
            )
              when ProcessId.eq(processId, event.processId) =>
            ProposeCustodian(partnerProcess)
          | (ProposeCustodian(_), CustodianProposed(event))
              when UserId.eq(event.data.partnerId, creatorId) =>
            EndorseCustodian(event.processId)
          | (EndorseCustodian(processId), CustodianEndorsed(event))
              when ProcessId.eq(processId, event.processId) =>
            Complete
          | _ => state^
          }
        );
      result :=
        (
          switch (state^) {
          | ProposePartner =>
            Some((
              issuerKeyPair,
              Event.makePartnerProposed(
                ~eligibleWhenProposing=
                  [|creatorId|] |> Belt.Set.mergeMany(UserId.emptySet),
                ~proposerId=creatorId,
                ~prospectId=creatorId,
                ~prospectPubKey=creatorPubKey,
                ~policy=
                  initialPolicies
                  |> Utils.mapOption((p: VentureCreated.initialPolicies) =>
                       p.addPartner
                     )
                  |> Js.Option.getWithDefault(Policy.defaultAddPartner),
                ~lastRemovalAccepted=None,
                (),
              ),
            ))
          | EndorsePartner({processId}) =>
            Some((
              issuerKeyPair,
              Event.makePartnerEndorsed(~processId, ~supporterId=creatorId),
            ))
          | ProposeAccountCreation(_) =>
            Some((
              issuerKeyPair,
              Event.makeAccountCreationProposed(
                ~eligibleWhenProposing=
                  [|creatorId|] |> Belt.Set.mergeMany(UserId.emptySet),
                ~proposerId=creatorId,
                ~name=defaultAccountName,
                ~accountIdx=AccountIndex.default,
                ~accountSettings=
                  defaultAccountSettings
                  |> Js.Option.getWithDefault(AccountSettings.default),
                ~policy=metaPolicy,
              ),
            ))
          | EndorseAccountCreation(processId, _) =>
            Some((
              issuerKeyPair,
              Event.makeAccountCreationEndorsed(
                ~processId,
                ~supporterId=creatorId,
              ),
            ))
          | ProposeCustodian(partnerProposed) =>
            Some((
              issuerKeyPair,
              Event.makeCustodianProposed(
                ~eligibleWhenProposing=
                  [|creatorId|] |> Belt.Set.mergeMany(UserId.emptySet),
                ~lastCustodianRemovalAccepted=None,
                ~partnerProposed,
                ~proposerId=creatorId,
                ~accountIdx=AccountIndex.default,
                ~policy=
                  initialPolicies
                  |> Utils.mapOption((p: VentureCreated.initialPolicies) =>
                       p.addPartner
                     )
                  |> Js.Option.getWithDefault(Policy.defaultAddPartner),
              ),
            ))
          | EndorseCustodian(processId) =>
            Some((
              issuerKeyPair,
              Event.makeCustodianEndorsed(~processId, ~supporterId=creatorId),
            ))
          | PartnerEndorsed(_)
          | AccountCreationEndorsed(_)
          | Complete => None
          }
        );
    };
    pub processCompleted = () => state^ == Complete;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
