open PrimitiveTypes;

module VentureCreated = {
  type t = {
    ventureId,
    ventureName: string,
    creatorId: userId,
    creatorPubKey: string,
    metaPolicy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t
  };
  let make = (~ventureName, ~creatorId, ~creatorPubKey, ~metaPolicy) => {
    ventureId: VentureId.make(),
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
        ("ventureId", VentureId.encode(event.ventureId)),
        ("ventureName", string(event.ventureName)),
        ("creatorId", UserId.encode(event.creatorId)),
        ("creatorPubKey", string(event.creatorPubKey)),
        ("metaPolicy", Policy.encode(event.metaPolicy)),
        ("systemIssuer", string(Bitcoin.ECPair.toWIF(event.systemIssuer)))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      ventureId: raw |> field("ventureId", VentureId.decode),
      ventureName: raw |> field("ventureName", string),
      creatorId: raw |> field("creatorId", UserId.decode),
      creatorPubKey: raw |> field("creatorPubKey", string),
      metaPolicy: raw |> field("metaPolicy", Policy.decode),
      systemIssuer:
        raw |> field("systemIssuer", string) |> Bitcoin.ECPair.fromWIF
    };
};

module ProspectSuggested = {
  type t = {
    processId,
    supporterId: userId,
    prospectId: userId,
    prospectPubKey: string,
    policy: Policy.t
  };
  let make = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) => {
    processId: ProcessId.make(),
    supporterId,
    prospectId,
    prospectPubKey,
    policy
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProspectSuggested")),
        ("processId", ProcessId.encode(event.processId)),
        ("supporterId", UserId.encode(event.supporterId)),
        ("prospectId", UserId.encode(event.prospectId)),
        ("prospectPubKey", string(event.prospectPubKey)),
        ("policy", Policy.encode(event.policy))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      supporterId: raw |> field("supporterId", UserId.decode),
      prospectId: raw |> field("prospectId", UserId.decode),
      prospectPubKey: raw |> field("prospectPubKey", string),
      policy: raw |> field("policy", Policy.decode)
    };
};

module ProspectApproved = {
  type t = {
    processId,
    supporterId: userId
  };
  let make = (~processId, ~supporterId) => {processId, supporterId};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProspectApproved")),
        ("processId", ProcessId.encode(event.processId)),
        ("supporterId", UserId.encode(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      supporterId: raw |> field("supporterId", UserId.decode)
    };
};

module PartnerAdded = {
  type t = {
    processId,
    partnerId: userId,
    pubKey: string
  };
  let make = (~processId, ~partnerId, ~pubKey) => {
    processId,
    partnerId,
    pubKey
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerAdded")),
        ("processId", ProcessId.encode(event.processId)),
        ("partnerId", UserId.encode(event.partnerId)),
        ("pubKey", string(event.pubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      partnerId: raw |> field("partnerId", UserId.decode),
      pubKey: raw |> field("pubKey", string)
    };
};

module PartnerLabelSuggested = {
  type t = {
    processId,
    partnerId: userId,
    labelId,
    supporterId: userId
  };
  let make = (~partnerId, ~labelId, ~supporterId) => {
    processId: ProcessId.make(),
    partnerId,
    labelId,
    supporterId
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerLabelSuggested")),
        ("processId", ProcessId.encode(event.processId)),
        ("partnerId", UserId.encode(event.partnerId)),
        ("labelId", LabelId.encode(event.labelId)),
        ("supporterId", UserId.encode(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      partnerId: raw |> field("partnerId", UserId.decode),
      labelId: raw |> field("labelId", LabelId.decode),
      supporterId: raw |> field("supporterId", UserId.decode)
    };
};

module PartnerLabelApproved = {
  type t = {
    processId,
    supporterId: userId
  };
  let make = (~processId, ~supporterId) => {processId, supporterId};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerLabelApproved")),
        ("processId", ProcessId.encode(event.processId)),
        ("supporterId", UserId.encode(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      supporterId: raw |> field("supporterId", UserId.decode)
    };
};

module PartnerLabelAccepted = {
  type t = {
    processId,
    userId,
    labelId,
    supporterId: userId
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerLabelAccepted")),
        ("processId", ProcessId.encode(event.processId)),
        ("userId", UserId.encode(event.userId)),
        ("labelId", LabelId.encode(event.labelId)),
        ("supporterId", UserId.encode(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      userId: raw |> field("userId", UserId.decode),
      labelId: raw |> field("labelId", LabelId.decode),
      supporterId: raw |> field("supporterId", UserId.decode)
    };
};

module ContributionSubmitted = {
  type t = {
    processId,
    submitterId: userId,
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
    processId: ProcessId.make(),
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
        ("processId", ProcessId.encode(event.processId)),
        ("submitterId", UserId.encode(event.submitterId)),
        ("amountInteger", int(event.amountInteger)),
        ("amountFraction", int(event.amountFraction)),
        ("currency", string(event.currency)),
        ("description", string(event.description)),
        ("policy", Policy.encode(event.policy))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      submitterId: raw |> field("submitterId", UserId.decode),
      amountInteger: raw |> field("amountInteger", int),
      amountFraction: raw |> field("amountFraction", int),
      currency: raw |> field("currency", string),
      description: raw |> field("description", string),
      policy: raw |> field("policy", Policy.decode)
    };
};

module ContributionApproved = {
  type t = {
    processId,
    supporterId: userId
  };
  let make = (~processId, ~supporterId) => {processId, supporterId};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionApproved")),
        ("processId", ProcessId.encode(event.processId)),
        ("supporterId", UserId.encode(event.supporterId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      supporterId: raw |> field("supporterId", UserId.decode)
    };
};

module ContributionAccepted = {
  type t = {processId};
  let make = (~processId) => {processId: processId};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ContributionAccepted")),
        ("processId", ProcessId.encode(event.processId))
      ])
    );
  let decode = raw =>
    Json.Decode.{processId: raw |> field("processId", ProcessId.decode)};
};

type t =
  | VentureCreated(VentureCreated.t)
  | ProspectSuggested(ProspectSuggested.t)
  | ProspectApproved(ProspectApproved.t)
  | PartnerAdded(PartnerAdded.t)
  | PartnerLabelSuggested(PartnerLabelSuggested.t)
  | PartnerLabelApproved(PartnerLabelApproved.t)
  | PartnerLabelAccepted(PartnerLabelAccepted.t)
  | ContributionSubmitted(ContributionSubmitted.t)
  | ContributionApproved(ContributionApproved.t)
  | ContributionAccepted(ContributionAccepted.t);

let makeProspectSuggested =
    (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  ProspectSuggested(
    ProspectSuggested.make(~supporterId, ~prospectId, ~prospectPubKey, ~policy)
  );

let makeProspectApproved = (~processId, ~supporterId) =>
  ProspectApproved(ProspectApproved.make(~processId, ~supporterId));

let makePartnerLabelSuggested = (~partnerId, ~labelId, ~supporterId) =>
  PartnerLabelSuggested(
    PartnerLabelSuggested.make(~partnerId, ~labelId, ~supporterId)
  );

let makePartnerLabelApproved = (~processId, ~supporterId) =>
  PartnerLabelApproved(PartnerLabelApproved.make(~processId, ~supporterId));

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

let makeContributionApproved = (~processId, ~supporterId) =>
  ContributionApproved(ContributionApproved.make(~processId, ~supporterId));

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | ProspectSuggested(event) => ProspectSuggested.encode(event)
  | ProspectApproved(event) => ProspectApproved.encode(event)
  | PartnerAdded(event) => PartnerAdded.encode(event)
  | PartnerLabelSuggested(event) => PartnerLabelSuggested.encode(event)
  | PartnerLabelApproved(event) => PartnerLabelApproved.encode(event)
  | PartnerLabelAccepted(event) => PartnerLabelAccepted.encode(event)
  | ContributionSubmitted(event) => ContributionSubmitted.encode(event)
  | ContributionApproved(event) => ContributionApproved.encode(event)
  | ContributionAccepted(event) => ContributionAccepted.encode(event);

let isSystemEvent =
  fun
  | PartnerAdded(_)
  | PartnerLabelAccepted(_)
  | ContributionAccepted(_) => true
  | _ => false;

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "VentureCreated" => VentureCreated(VentureCreated.decode(raw))
  | "ProspectSuggested" => ProspectSuggested(ProspectSuggested.decode(raw))
  | "ProspectApproved" => ProspectApproved(ProspectApproved.decode(raw))
  | "PartnerAdded" => PartnerAdded(PartnerAdded.decode(raw))
  | "PartnerLabelSuggested" =>
    PartnerLabelSuggested(PartnerLabelSuggested.decode(raw))
  | "PartnerLabelApproved" =>
    PartnerLabelApproved(PartnerLabelApproved.decode(raw))
  | "PartnerLabelAccepted" =>
    PartnerLabelAccepted(PartnerLabelAccepted.decode(raw))
  | "ContributionSubmitted" =>
    ContributionSubmitted(ContributionSubmitted.decode(raw))
  | "ContributionApproved" =>
    ContributionApproved(ContributionApproved.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(ContributionAccepted.decode(raw))
  | _ => raise(Not_found)
  };
};
