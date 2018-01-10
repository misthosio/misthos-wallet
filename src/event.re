module ProjectCreated = {
  type t = {
    projectId: string,
    projectName: string,
    creatorId: string,
    creatorPubKey: string,
    creatorStorageUrlPrefix: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProjectCreated")),
        ("projectId", string(event.projectId)),
        ("projectName", string(event.projectName)),
        ("creatorId", string(event.creatorId)),
        ("creatorPubKey", string(event.creatorPubKey)),
        ("creatorStorageUrlPrefix", string(event.creatorStorageUrlPrefix))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      projectId: raw |> field("projectId", string),
      projectName: raw |> field("projectName", string),
      creatorId: raw |> field("creatorId", string),
      creatorPubKey: raw |> field("creatorPubKey", string),
      creatorStorageUrlPrefix: raw |> field("creatorStorageUrlPrefix", string)
    };
};

module CandidateSuggested = {
  type t = {
    candidateId: string,
    candidatePubKey: string,
    candidateStorageUrlPrefix: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("CandidateSuggested")),
        ("candidateId", string(event.candidateId)),
        ("candidatePubKey", string(event.candidatePubKey)),
        ("candidateStorageUrlPrefix", string(event.candidateStorageUrlPrefix))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      candidateId: raw |> field("candidateId", string),
      candidatePubKey: raw |> field("candidatePubKey", string),
      candidateStorageUrlPrefix:
        raw |> field("candidateStorageUrlPrefix", string)
    };
};

type t =
  | ProjectCreated(ProjectCreated.t)
  | CandidateSuggested(CandidateSuggested.t);

let encode = event =>
  switch event {
  | ProjectCreated(event) => ProjectCreated.encode(event)
  | CandidateSuggested(event) => CandidateSuggested.encode(event)
  };

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "ProjectCreated" => ProjectCreated(ProjectCreated.decode(raw))
  | "CandidateSuggested" => CandidateSuggested(CandidateSuggested.decode(raw))
  };
};
