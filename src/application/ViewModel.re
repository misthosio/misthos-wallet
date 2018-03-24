open PrimitiveTypes;

type partnerLabel = {
  processId,
  userId,
  labelId,
  supporters: list(userId)
};

type partner = {
  userId,
  labels: list(labelId)
};

type prospect = {
  processId,
  userId,
  endorsedBy: list(userId)
};

type t = {
  name: string,
  partners: list(partner),
  partnerLabelProcesses: list((processId, partnerLabel)),
  prospects: list(prospect),
  metaPolicy: Policy.t,
  partnerPolicy: Policy.t,
  partnerLabelPolicy: Policy.t
};

let make = () => {
  name: "",
  partners: [],
  prospects: [],
  partnerLabelProcesses: [],
  metaPolicy: Policy.absolute,
  partnerPolicy: Policy.absolute,
  partnerLabelPolicy: Policy.absolute
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureName, metaPolicy}) => {
      ...state,
      name: ventureName,
      metaPolicy,
      partnerPolicy: metaPolicy,
      partnerLabelPolicy: metaPolicy
    }
  | PartnerEndorsed({processId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |> List.map((p: prospect) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           )
    }
  | PartnerProposed({processId, supporterId, data}) => {
      ...state,
      prospects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.prospects
      ]
    }
  | PartnerAccepted({data}) => {
      ...state,
      partners: [{userId: data.id, labels: []}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, data.id))
    }
  | PartnerLabelProposed({processId, supporterId, data}) => {
      ...state,
      partnerLabelProcesses: [
        (
          processId,
          {
            processId,
            userId: data.partnerId,
            labelId: data.labelId,
            supporters: [supporterId]
          }
        ),
        ...state.partnerLabelProcesses
      ]
    }
  | PartnerLabelEndorsed({processId, supporterId}) => {
      ...state,
      partnerLabelProcesses:
        state.partnerLabelProcesses
        |> List.map(((pId, labelProcess: partnerLabel)) =>
             ProcessId.eq(pId, processId) ?
               (
                 processId,
                 {
                   ...labelProcess,
                   supporters: [supporterId, ...labelProcess.supporters]
                 }
               ) :
               (processId, labelProcess)
           )
    }
  | PartnerLabelAccepted({processId, data}) => {
      ...state,
      partners:
        state.partners
        |> List.map((partner: partner) =>
             UserId.eq(partner.userId, data.partnerId) ?
               {...partner, labels: [data.labelId, ...partner.labels]} :
               partner
           ),
      partnerLabelProcesses:
        state.partnerLabelProcesses
        |> List.filter(((pId, _)) => ProcessId.neq(pId, processId))
    }
  | CustodianProposed(_)
  | CustodianEndorsed(_)
  | CustodianAccepted(_) => state
  };

let getPartners = state => state.partners;

let getPendingPartnerLabels = (partnerId, state) =>
  state.partnerLabelProcesses
  |> List.map(snd)
  |> List.find_all((partnerLabels: partnerLabel) =>
       UserId.eq(partnerLabels.userId, partnerId)
     );

let getProspects = state => state.prospects;

let ventureName = state => state.name;
