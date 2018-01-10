module ProjectCreated = {
  type t = {
    projectId: string,
    projectName: string,
    creatorId: string,
    creatorPubKey: string
  };
  let make = (~projectName, ~creatorId, ~creatorPubKey) => {
    projectId: Uuid.v4(),
    projectName,
    creatorId,
    creatorPubKey
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProjectCreated")),
        ("projectId", string(event.projectId)),
        ("projectName", string(event.projectName)),
        ("creatorId", string(event.creatorId)),
        ("creatorPubKey", string(event.creatorPubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      projectId: raw |> field("projectId", string),
      projectName: raw |> field("projectName", string),
      creatorId: raw |> field("creatorId", string),
      creatorPubKey: raw |> field("creatorPubKey", string)
    };
};

module CandidateSuggested = {
  type t = {
    processId: string,
    candidateId: string,
    candidatePubKey: string
  };
  let make = (~candidateId, ~candidatePubKey) => {
    processId: Uuid.v4(),
    candidateId,
    candidatePubKey
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("CandidateSuggested")),
        ("processId", string(event.processId)),
        ("candidateId", string(event.candidateId)),
        ("candidatePubKey", string(event.candidatePubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      candidateId: raw |> field("candidateId", string),
      candidatePubKey: raw |> field("candidatePubKey", string)
    };
};

module CandidateApproved = {
  type t = {
    processId: string,
    candidateId: string,
    supporterId: string
  };
  let make = (~processId, ~candidateId, ~supporterId) => {
    processId,
    candidateId,
    supporterId
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("CandidateApproved")),
        ("processId", string(event.processId)),
        ("candidateId", string(event.candidateId)),
        ("supporterId", string(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      candidateId: raw |> field("candidateId", string),
      supporterId: raw |> field("supporterId", string)
    };
};

module MemberAdded = {
  type t = {
    processId: string,
    blockstackId: string,
    pubKey: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("MemberAdded")),
        ("processId", string(event.processId)),
        ("blockstackId", string(event.blockstackId)),
        ("pubKey", string(event.pubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      blockstackId: raw |> field("blockstackId", string),
      pubKey: raw |> field("pubKey", string)
    };
};

module ContributionSubmitted = {
  type t = {
    processId: string,
    amount: float,
    description: string
  };
  let make = (~amount, ~description) => {
    processId: Uuid.v4(),
    amount,
    description
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionSubmitted")),
        ("processId", string(event.processId)),
        ("amount", float(event.amount)),
        ("description", string(event.description))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      amount: raw |> field("amount", float),
      description: raw |> field("description", string)
    };
};

module ContributionApproved = {
  type t = {
    processId: string,
    supporterId: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionApproved")),
        ("processId", string(event.processId)),
        ("supporterId", string(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      supporterId: raw |> field("supporterId", string)
    };
};

module ContributionAccepted = {
  type t = {processId: string};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionAccepted")),
        ("processId", string(event.processId))
      ])
    );
  let decode = raw =>
    Json.Decode.{processId: raw |> field("processId", string)};
};

type t =
  | ProjectCreated(ProjectCreated.t)
  | CandidateSuggested(CandidateSuggested.t)
  | CandidateApproved(CandidateApproved.t)
  | MemberAdded(MemberAdded.t)
  | ContributionSubmitted(ContributionSubmitted.t)
  | ContributionApproved(ContributionApproved.t)
  | ContributionAccepted(ContributionAccepted.t);

let makeCandidateSuggested = (~candidateId, ~candidatePubKey) =>
  CandidateSuggested(CandidateSuggested.make(~candidateId, ~candidatePubKey));

let encode = event =>
  switch event {
  | ProjectCreated(event) => ProjectCreated.encode(event)
  | CandidateSuggested(event) => CandidateSuggested.encode(event)
  | CandidateApproved(event) => CandidateApproved.encode(event)
  | MemberAdded(event) => MemberAdded.encode(event)
  | ContributionSubmitted(event) => ContributionSubmitted.encode(event)
  | ContributionApproved(event) => ContributionApproved.encode(event)
  | ContributionAccepted(event) => ContributionAccepted.encode(event)
  };

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "ProjectCreated" => ProjectCreated(ProjectCreated.decode(raw))
  | "CandidateSuggested" => CandidateSuggested(CandidateSuggested.decode(raw))
  | "CandidateApproved" => CandidateApproved(CandidateApproved.decode(raw))
  | "MemberAdded" => MemberAdded(MemberAdded.decode(raw))
  | "ContributionSubmitted" =>
    ContributionSubmitted(ContributionSubmitted.decode(raw))
  | "ContributionApproved" =>
    ContributionApproved(ContributionApproved.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(ContributionAccepted.decode(raw))
  };
};
