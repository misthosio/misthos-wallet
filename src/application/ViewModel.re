open PrimitiveTypes;

type partner = {userId};

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
  metaPolicy: Policy.absolute,
  addPartnerPolicy: Policy.absolute,
  acceptContributionPolicy: Policy.absolute
};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureName, creatorId, metaPolicy}) => {
      ...state,
      name: ventureName,
      partners: [{userId: creatorId}],
      metaPolicy,
      addPartnerPolicy: metaPolicy,
      acceptContributionPolicy: metaPolicy
    }
  | ProspectEndorsed({processId, supporterId}) => {
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
  | PartnerAdded({partnerId}) => {
      ...state,
      partners: [{userId: partnerId}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, partnerId))
    }
  | ContributionSubmitted({
      processId,
      submitterId,
      amountInteger,
      amountFraction,
      description
    }) => {
      ...state,
      contributions: [
        {
          processId,
          amountInteger,
          amountFraction,
          description,
          accepted: false,
          supporters: [submitterId]
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
  | PartnerLabelSuggested(_)
  | PartnerLabelEndorsed(_)
  | PartnerLabelAccepted(_) => state
  };

let getPartners = state => state.partners;

let getProspects = state => state.prospects;

let getContributions = state => state.contributions;

let ventureName = state => state.name;
