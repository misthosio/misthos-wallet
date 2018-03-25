open Event;

open PrimitiveTypes;

let defaultAccountName = "default";

let defaultAccountIndex = 0;

type state =
  | ProposePartner
  | PartnerProposed(processId)
  | ProposeAccountCreation
  | AccountCreationProposed(processId)
  | ProposeCustodian
  | CustodianProposed(processId)
  | Complete;

let make =
    (
      {userId, appKeyPair}: Session.Data.t,
      {creatorId, creatorPubKey, metaPolicy}: VentureCreated.t,
      log
    ) => {
  let process = {
    val state = ref(UserId.eq(userId, creatorId) ? ProposePartner : Complete);
    val result = ref(None);
    pub receive = ({event}: EventLog.item) => {
      state :=
        (
          switch (state^, event) {
          | (ProposePartner, PartnerProposed(event))
              when UserId.eq(event.data.id, creatorId) =>
            PartnerProposed(event.processId)
          | (PartnerProposed(processId), PartnerAccepted(event))
              when ProcessId.eq(processId, event.processId) =>
            ProposeAccountCreation
          | (ProposeAccountCreation, AccountCreationProposed(event))
              when event.data.accountIndex == defaultAccountIndex =>
            AccountCreationProposed(event.processId)
          | (
              AccountCreationProposed(processId),
              AccountCreationAccepted(event)
            )
              when ProcessId.eq(processId, event.processId) =>
            ProposeCustodian
          | (ProposeCustodian, CustodianProposed(event))
              when UserId.eq(event.data.partnerId, creatorId) =>
            CustodianProposed(event.processId)
          | (CustodianProposed(processId), CustodianAccepted(event))
              when ProcessId.eq(processId, event.processId) =>
            Complete
          | _ => state^
          }
        );
      result :=
        (
          switch state^ {
          | ProposePartner =>
            Some((
              appKeyPair,
              Event.makePartnerProposed(
                ~supporterId=creatorId,
                ~prospectId=creatorId,
                ~prospectPubKey=creatorPubKey,
                ~policy=metaPolicy
              )
            ))
          | ProposeAccountCreation =>
            Some((
              appKeyPair,
              Event.makeAccountCreationProposed(
                ~supporterId=creatorId,
                ~name=defaultAccountName,
                ~accountIndex=defaultAccountIndex,
                ~policy=metaPolicy
              )
            ))
          | ProposeCustodian =>
            Some((
              appKeyPair,
              Event.makeCustodianProposed(
                ~partnerId=creatorId,
                ~supporterId=creatorId,
                ~accountIndex=defaultAccountIndex,
                ~policy=metaPolicy
              )
            ))
          | _ => None
          }
        );
    };
    pub processCompleted = () => state^ == Complete;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
