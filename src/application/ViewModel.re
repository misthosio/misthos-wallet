type partner = {blockstackId: string};

type prospect = {
  blockstackId: string,
  approvedBy: list(string)
};

type contribution = {
  processId: string,
  amountInteger: int,
  amountFraction: int,
  description: string,
  supporters: list(string),
  accepted: bool
};

type t = {
  name: string,
  partners: list(partner),
  prospects: list(prospect),
  contributions: list(contribution)
};

let make = () => {name: "", partners: [], prospects: [], contributions: []};

let apply = (event: Event.t, state) =>
  switch event {
  | VentureCreated({ventureName, creatorId}) => {
      ...state,
      name: ventureName,
      partners: [{blockstackId: creatorId}]
    }
  | ProspectApproved({prospectId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |> List.map(p =>
             if (p.blockstackId == prospectId) {
               {...p, approvedBy: [supporterId, ...p.approvedBy]};
             } else {
               p;
             }
           )
    }
  | ProspectSuggested(event) => {
      ...state,
      prospects: [
        {blockstackId: event.prospectId, approvedBy: [event.supporterId]},
        ...state.prospects
      ]
    }
  | PartnerAdded({blockstackId}) => {
      ...state,
      partners: [{blockstackId: blockstackId}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => p.blockstackId != blockstackId)
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
  | ContributionApproved({processId, supporterId}) => {
      ...state,
      contributions:
        state.contributions
        |> List.map(c =>
             c.processId == processId ?
               {...c, supporters: [supporterId, ...c.supporters]} : c
           )
    }
  | ContributionAccepted({processId}) => {
      ...state,
      contributions:
        state.contributions
        |> List.map(c => c.processId == processId ? {...c, accepted: true} : c)
    }
  | _ => state
  };

let getPartners = state => state.partners;

let getProspects = state => state.prospects;

let getContributions = state => state.contributions;

let ventureName = state => state.name;
