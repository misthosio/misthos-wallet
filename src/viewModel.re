module Member = {
  type t = {blockstackId: string};
};

module Candidate = {
  type t = {
    blockstackId: string,
    approvedBy: list(string)
  };
};

type t = {
  name: string,
  candidates: list(Candidate.t)
};

let make = () => {name: "", candidates: []};

let apply = (event: Event.t, state) =>
  switch event {
  | ProjectCreated(event) => {...state, name: event.projectName}
  | CandidateSuggested(event) => {
      ...state,
      candidates: [
        {blockstackId: event.candidateId, approvedBy: [event.supporterId]},
        ...state.candidates
      ]
    }
  | _ => state
  };

let getMembers = state => [];

let getCandidates = state => state.candidates;

let projectName = state => state.name;
