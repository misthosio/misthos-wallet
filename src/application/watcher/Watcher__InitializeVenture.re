open Event;

open PrimitiveTypes;

type state =
  | ProposePartner
  | PartnerProposed(processId)
  | ProposeLabels(list(labelId))
  | PartnerLabelProposed(processId, list(labelId))
  | ProposeCustodian
  | CustodianProposed(processId)
  | Complete;

let make =
    (
      {userId, appKeyPair}: Session.Data.t,
      {creatorId, creatorPubKey, initialLabelIds, metaPolicy}: VentureCreated.t,
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
            ProposeLabels(initialLabelIds)
          | (ProposeLabels(labelIds), PartnerLabelProposed(event))
              when LabelId.eq(labelIds |> List.hd, event.data.labelId) =>
            PartnerLabelProposed(event.processId, labelIds |> List.tl)
          | (
              PartnerLabelProposed(processId, labelIds),
              PartnerLabelAccepted(event)
            )
              when ProcessId.eq(processId, event.processId) =>
            switch labelIds {
            | [] => ProposeCustodian
            | _ => ProposeLabels(labelIds)
            }
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
          | ProposeLabels(labelIds) =>
            Some((
              appKeyPair,
              Event.makePartnerLabelProposed(
                ~partnerId=creatorId,
                ~labelId=labelIds |> List.hd,
                ~supporterId=creatorId,
                ~policy=metaPolicy
              )
            ))
          | ProposeCustodian =>
            Some((
              appKeyPair,
              Event.makeCustodianProposed(
                ~partnerId=creatorId,
                ~supporterId=creatorId,
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
