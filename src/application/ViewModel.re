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

type contribution = {
  processId,
  amountInteger: int,
  amountFraction: int,
  description: string,
  supporters: list(userId),
  accepted: bool
};

type t = {
  name: string,
  partners: list(partner),
  partnerLabelProcesses: list((processId, partnerLabel)),
  prospects: list(prospect),
  contributions: list(contribution),
  metaPolicy: Policy.t,
  addPartnerPolicy: Policy.t,
  acceptContributionPolicy: Policy.t
};

let make = () => {
  name: "",
  partners: [],
  prospects: [],
  contributions: [],
  partnerLabelProcesses: [],
  metaPolicy: Policy.absolute,
  addPartnerPolicy: Policy.absolute,
  acceptContributionPolicy: Policy.absolute
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureName, creatorId, metaPolicy}) => {
      ...state,
      name: ventureName,
      partners: [{userId: creatorId, labels: []}],
      metaPolicy,
      addPartnerPolicy: metaPolicy,
      acceptContributionPolicy: metaPolicy
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
  | ContributionProposed({processId, supporterId, data}) => {
      ...state,
      contributions: [
        {
          processId,
          amountInteger: data.amountInteger,
          amountFraction: data.amountFraction,
          description: data.description,
          accepted: false,
          supporters: [supporterId]
        },
        ...state.contributions
      ]
    }
  | ContributionEndorsed({processId, supporterId}) => {
      ...state,
      contributions:
        state.contributions
        |> List.map(c =>
             ProcessId.eq(c.processId, processId) ?
               {...c, supporters: [supporterId, ...c.supporters]} : c
           )
    }
  | ContributionAccepted({processId}) => {
      ...state,
      contributions:
        state.contributions
        |> List.map(c =>
             ProcessId.eq(c.processId, processId) ? {...c, accepted: true} : c
           )
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
  };

let getPartners = state => state.partners;

let getPendingPartnerLabels = (partnerId, state) =>
  state.partnerLabelProcesses
  |> List.map(snd)
  |> List.find_all((partnerLabels: partnerLabel) =>
       UserId.eq(partnerLabels.userId, partnerId)
     );

let getProspects = state => state.prospects;

let getContributions = state => state.contributions;

let ventureName = state => state.name;
