type partner = {blockstackId: string};

type prospect = {
  blockstackId: string,
  approvedBy: list(string)
};

type t = {
  name: string,
  partners: list(partner),
  prospects: list(prospect)
};

let make = () => {name: "", partners: [], prospects: []};

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
  | _ => state
  };

let getPartners = state => state.partners;

let getProspects = state => state.prospects;

let ventureName = state => state.name;
