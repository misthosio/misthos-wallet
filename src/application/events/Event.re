module VentureCreated = {
  type t = {
    ventureId: string,
    ventureName: string,
    creatorId: string,
    creatorPubKey: string,
    metaPolicy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t
  };
  let make = (~ventureName, ~creatorId, ~creatorPubKey, ~metaPolicy) => {
    ventureId: Uuid.v4(),
    ventureName,
    creatorId,
    creatorPubKey,
    metaPolicy,
    systemIssuer: Bitcoin.ECPair.makeRandom()
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("VentureCreated")),
        ("ventureId", string(event.ventureId)),
        ("ventureName", string(event.ventureName)),
        ("creatorId", string(event.creatorId)),
        ("creatorPubKey", string(event.creatorPubKey)),
        ("metaPolicy", Policy.encode(event.metaPolicy)),
        ("systemIssuer", string(Bitcoin.ECPair.toWIF(event.systemIssuer)))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      ventureId: raw |> field("ventureId", string),
      ventureName: raw |> field("ventureName", string),
      creatorId: raw |> field("creatorId", string),
      creatorPubKey: raw |> field("creatorPubKey", string),
      metaPolicy: raw |> field("metaPolicy", Policy.decode),
      systemIssuer:
        raw |> field("systemIssuer", string) |> Bitcoin.ECPair.fromWIF
    };
};

module ProspectSuggested = {
  type t = {
    processId: string,
    supporterId: string,
    prospectId: string,
    prospectPubKey: string,
    policy: Policy.t
  };
  let make = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) => {
    processId: Uuid.v4(),
    supporterId,
    prospectId,
    prospectPubKey,
    policy
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProspectSuggested")),
        ("processId", string(event.processId)),
        ("supporterId", string(event.supporterId)),
        ("prospectId", string(event.prospectId)),
        ("prospectPubKey", string(event.prospectPubKey)),
        ("policy", Policy.encode(event.policy))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      supporterId: raw |> field("supporterId", string),
      prospectId: raw |> field("prospectId", string),
      prospectPubKey: raw |> field("prospectPubKey", string),
      policy: raw |> field("policy", Policy.decode)
    };
};

module ProspectApproved = {
  type t = {
    processId: string,
    prospectId: string,
    supporterId: string
  };
  let make = (~processId, ~prospectId, ~supporterId) => {
    processId,
    prospectId,
    supporterId
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProspectApproved")),
        ("processId", string(event.processId)),
        ("prospectId", string(event.prospectId)),
        ("supporterId", string(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      prospectId: raw |> field("prospectId", string),
      supporterId: raw |> field("supporterId", string)
    };
};

module PartnerAdded = {
  type t = {
    processId: string,
    blockstackId: string,
    pubKey: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerAdded")),
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
    submitterId: string,
    amountInteger: int,
    amountFraction: int,
    currency: string,
    description: string,
    policy: Policy.t
  };
  let make =
      (
        ~submitterId,
        ~amountInteger,
        ~amountFraction,
        ~currency,
        ~description,
        ~policy
      ) => {
    processId: Uuid.v4(),
    submitterId,
    amountInteger,
    amountFraction,
    currency,
    description,
    policy
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionSubmitted")),
        ("processId", string(event.processId)),
        ("submitterId", string(event.submitterId)),
        ("amountInteger", int(event.amountInteger)),
        ("amountFraction", int(event.amountFraction)),
        ("currency", string(event.currency)),
        ("description", string(event.description)),
        ("policy", Policy.encode(event.policy))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      submitterId: raw |> field("submitterId", string),
      amountInteger: raw |> field("amountInteger", int),
      amountFraction: raw |> field("amountFraction", int),
      currency: raw |> field("currency", string),
      description: raw |> field("description", string),
      policy: raw |> field("policy", Policy.decode)
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
  | VentureCreated(VentureCreated.t)
  | ProspectSuggested(ProspectSuggested.t)
  | ProspectApproved(ProspectApproved.t)
  | PartnerAdded(PartnerAdded.t)
  | ContributionSubmitted(ContributionSubmitted.t)
  | ContributionApproved(ContributionApproved.t)
  | ContributionAccepted(ContributionAccepted.t);

let makeProspectSuggested =
    (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  ProspectSuggested(
    ProspectSuggested.make(~supporterId, ~prospectId, ~prospectPubKey, ~policy)
  );

let makeProspectApproved = (~processId, ~prospectId, ~supporterId) =>
  ProspectApproved(
    ProspectApproved.make(~processId, ~prospectId, ~supporterId)
  );

let makeContributionSubmitted =
    (
      ~submitterId,
      ~amountInteger,
      ~amountFraction,
      ~currency,
      ~description,
      ~policy
    ) =>
  ContributionSubmitted(
    ContributionSubmitted.make(
      ~submitterId,
      ~amountInteger,
      ~amountFraction,
      ~currency,
      ~description,
      ~policy
    )
  );

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | ProspectSuggested(event) => ProspectSuggested.encode(event)
  | ProspectApproved(event) => ProspectApproved.encode(event)
  | PartnerAdded(event) => PartnerAdded.encode(event)
  | ContributionSubmitted(event) => ContributionSubmitted.encode(event)
  | ContributionApproved(event) => ContributionApproved.encode(event)
  | ContributionAccepted(event) => ContributionAccepted.encode(event);

let isSystemEvent =
  fun
  | PartnerAdded(_)
  | ContributionAccepted(_) => true
  | _ => false;

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "VentureCreated" => VentureCreated(VentureCreated.decode(raw))
  | "ProspectSuggested" => ProspectSuggested(ProspectSuggested.decode(raw))
  | "ProspectApproved" => ProspectApproved(ProspectApproved.decode(raw))
  | "PartnerAdded" => PartnerAdded(PartnerAdded.decode(raw))
  | "ContributionSubmitted" =>
    ContributionSubmitted(ContributionSubmitted.decode(raw))
  | "ContributionApproved" =>
    ContributionApproved(ContributionApproved.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(ContributionAccepted.decode(raw))
  | _ => raise(Not_found)
  };
};
