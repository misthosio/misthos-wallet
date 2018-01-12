type t = string;

let make = () => "";

let apply = (event: Event.t, state) => state;

/* switch event { */
/* | ProjectCreated(created) => { */
/*     ...state, */
/*     id: created.projectId, */
/*     name: created.projectName, */
/*     members: [ */
/*       ( */
/*         created.creatorPubKey, */
/*         {blockstackId: created.creatorId, pubKey: created.creatorPubKey} */
/*       ) */
/*     ] */
/*   } */
/* | CandidateSuggested(suggestion) => { */
/*     ...state, */
/*     candidates: [ */
/*       ( */
/*         suggestion.candidateId, */
/*         { */
/*           blockstackId: suggestion.candidateId, */
/*           pubKey: suggestion.candidatePubKey, */
/*           approvedBy: [memberIdFromPubKey(issuerPubKey, state)] */
/*         } */
/*       ), */
/*       ...state.candidates */
/*     ] */
/*   } */
/* | CandidateApproved(approval) => */
/*   let candidates = */
/*     state.candidates */
/*     |> List.map(((id, c)) => */
/*          if (id == approval.candidateId) { */
/*            (id, {...c, approvedBy: [approval.supporterId, ...c.approvedBy]}); */
/*          } else { */
/*            (id, c); */
/*          } */
/*        ); */
/*   {...state, candidates}; */
module Member = {
  type t = {blockstackId: string};
};

module Candidate = {
  type t = {
    blockstackId: string,
    approvedBy: list(string)
  };
};

let getMembers = state => [];

let getCandidates = state => [];

let projectName = state => "";
