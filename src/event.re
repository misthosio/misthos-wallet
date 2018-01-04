type t =
  | ProjectCreated
  | CandidateSubmited
  | Unknown;

let getType = event =>
  switch event {
  | ProjectCreated => "ProjectCreated"
  | CandidateSubmited => "CandidateSubmited"
  | Unknown => "Unknown"
  };

let encode = event =>
  switch event {
  | ProjectCreated => "ProjectCreated"
  | CandidateSubmited => "CandidateSubmited"
  | Unknown => "Unknown"
  };

let decode = (_raw, ~type_) =>
  switch type_ {
  | "ProjectCreated" => ProjectCreated
  | "CandidateSubmited" => CandidateSubmited
  | _ => Unknown
  };

type validatorState =
  | State;

let canKeyWitnessItem = (state, item, pubKey) => true;

let canKeySignItem = (state, item, pubKey) => true;

let validate = (state, item) => (true, state);
