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

module Partner = {
  module Data = {
    type t = {
      id: userId,
      pubKey: string
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("id", UserId.encode(event.id)),
          ("pubKey", string(event.pubKey))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        id: raw |> field("id", UserId.decode),
        pubKey: raw |> field("pubKey", string)
      };
  };
  include (val EventTypes.makeProcess("Partner"))(Data);
};

module PartnerLabelSuggested = {
  type t = {
    processId,
    partnerId: userId,
    labelId,
    supporterId: userId,
    policy: Policy.t
  };
  let make = (~partnerId, ~labelId, ~supporterId, ~policy) => {
    processId: ProcessId.make(),
    partnerId,
    labelId,
    supporterId,
    policy
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerLabelSuggested")),
        ("processId", ProcessId.encode(event.processId)),
        ("partnerId", UserId.encode(event.partnerId)),
        ("labelId", LabelId.encode(event.labelId)),
        ("supporterId", UserId.encode(event.supporterId)),
        ("policy", Policy.encode(event.policy))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      partnerId: raw |> field("partnerId", UserId.decode),
      labelId: raw |> field("labelId", LabelId.decode),
      supporterId: raw |> field("supporterId", UserId.decode),
      policy: raw |> field("policy", Policy.decode)
    };
};

module PartnerLabelEndorsed = (
  val EventTypes.makeEndorsement("PartnerLabelEndorsed")
);

module PartnerLabelAccepted = {
  type t = {
    processId,
    partnerId: userId,
    labelId
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerLabelAccepted")),
        ("processId", ProcessId.encode(event.processId)),
        ("partnerId", UserId.encode(event.partnerId)),
        ("labelId", LabelId.encode(event.labelId))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      partnerId: raw |> field("partnerId", UserId.decode),
      labelId: raw |> field("labelId", LabelId.decode)
    };
};

module ContributionData = {
  type t = {
    amountInteger: int,
    amountFraction: int,
    currency: string,
    description: string
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("amountInteger", int(event.amountInteger)),
        ("amountFraction", int(event.amountFraction)),
        ("currency", string(event.currency)),
        ("description", string(event.description))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      amountInteger: raw |> field("amountInteger", int),
      amountFraction: raw |> field("amountFraction", int),
      currency: raw |> field("currency", string),
      description: raw |> field("description", string)
    };
};

module ContributionProposed =
  (val EventTypes.makeProposal("ContributionProposed"))(ContributionData);

module ContributionEndorsed = (
  val EventTypes.makeEndorsement("ContributionEndorsed")
);

module ContributionAccepted =
  (val EventTypes.makeAcceptance("ContributionAccepted"))(ContributionData);

type t =
  | VentureCreated(VentureCreated.t)
  | PartnerProposed(Partner.Proposal.t)
  | PartnerEndorsed(Partner.Endorsement.t)
  | PartnerAccepted(Partner.Acceptance.t)
  | PartnerLabelSuggested(PartnerLabelSuggested.t)
  | PartnerLabelEndorsed(PartnerLabelEndorsed.t)
  | PartnerLabelAccepted(PartnerLabelAccepted.t)
  | ContributionProposed(ContributionProposed.t)
  | ContributionEndorsed(ContributionEndorsed.t)
  | ContributionAccepted(ContributionAccepted.t);

let makePartnerProposed = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  PartnerProposed(
    Partner.Proposal.make(
      ~supporterId,
      ~policy,
      ~data=Partner.Data.{id: prospectId, pubKey: prospectPubKey}
    )
  );

let makePartnerEndorsed = (~processId, ~supporterId) =>
  PartnerEndorsed(Partner.Endorsement.make(~processId, ~supporterId));

let makePartnerLabelSuggested = (~partnerId, ~labelId, ~supporterId, ~policy) =>
  PartnerLabelSuggested(
    PartnerLabelSuggested.make(~partnerId, ~labelId, ~supporterId, ~policy)
  );

let makePartnerLabelEndorsed = (~processId, ~supporterId) =>
  PartnerLabelEndorsed(PartnerLabelEndorsed.make(~processId, ~supporterId));

let makeContributionProposed =
    (
      ~supporterId,
      ~amountInteger,
      ~amountFraction,
      ~currency,
      ~description,
      ~policy
    ) =>
  ContributionProposed(
    ContributionProposed.make(
      ~supporterId,
      ~policy,
      ~data=
        ContributionData.{amountInteger, amountFraction, currency, description}
    )
  );

let makeContributionEndorsed = (~processId, ~supporterId) =>
  ContributionEndorsed(ContributionEndorsed.make(~processId, ~supporterId));

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | PartnerProposed(event) => Partner.Proposal.encode(event)
  | PartnerEndorsed(event) => Partner.Endorsement.encode(event)
  | PartnerAccepted(event) => Partner.Acceptance.encode(event)
  | PartnerLabelSuggested(event) => PartnerLabelSuggested.encode(event)
  | PartnerLabelEndorsed(event) => PartnerLabelEndorsed.encode(event)
  | PartnerLabelAccepted(event) => PartnerLabelAccepted.encode(event)
  | ContributionProposed(event) => ContributionProposed.encode(event)
  | ContributionEndorsed(event) => ContributionEndorsed.encode(event)
  | ContributionAccepted(event) => ContributionAccepted.encode(event);

let isSystemEvent =
  fun
  | PartnerAccepted(_)
  | PartnerLabelAccepted(_)
  | ContributionAccepted(_) => true
  | _ => false;

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "VentureCreated" => VentureCreated(VentureCreated.decode(raw))
  | "PartnerProposed" => PartnerProposed(Partner.Proposal.decode(raw))
  | "PartnerEndorsed" => PartnerEndorsed(Partner.Endorsement.decode(raw))
  | "PartnerAccepted" => PartnerAccepted(Partner.Acceptance.decode(raw))
  | "PartnerLabelSuggested" =>
    PartnerLabelSuggested(PartnerLabelSuggested.decode(raw))
  | "PartnerLabelEndorsed" =>
    PartnerLabelEndorsed(PartnerLabelEndorsed.decode(raw))
  | "PartnerLabelAccepted" =>
    PartnerLabelAccepted(PartnerLabelAccepted.decode(raw))
  | "ContributionProposed" =>
    ContributionProposed(ContributionProposed.decode(raw))
  | "ContributionEndorsed" =>
    ContributionEndorsed(ContributionEndorsed.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(ContributionAccepted.decode(raw))
  | _ => raise(Not_found)
  };
};
