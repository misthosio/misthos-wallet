open PrimitiveTypes;

module VentureCreated = {
  type t = {
    ventureId,
    ventureName: string,
    creatorId: userId,
    creatorPubKey: string,
    metaPolicy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t,
    initialLabelIds: list(labelId),
    distribution: DistributionGraph.t
  };
  let make =
      (~ventureName, ~creatorId, ~creatorPubKey, ~metaPolicy, ~initialLabelIds) => {
    ventureId: VentureId.make(),
    ventureName,
    creatorId,
    creatorPubKey,
    metaPolicy,
    systemIssuer: Bitcoin.ECPair.makeRandom(),
    initialLabelIds,
    distribution: DistributionGraph.make(creatorId, initialLabelIds)
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
        ("systemIssuer", string(Bitcoin.ECPair.toWIF(event.systemIssuer))),
        ("initialLabelIds", list(LabelId.encode, event.initialLabelIds)),
        ("distributionGraph", DistributionGraph.encode(event.distribution))
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
        raw |> field("systemIssuer", string) |> Bitcoin.ECPair.fromWIF,
      initialLabelIds: raw |> field("initialLabelIds", list(LabelId.decode)),
      distribution: raw |> field("distributionGraph", DistributionGraph.decode)
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

module PartnerLabel = {
  module Data = {
    type t = {
      partnerId: userId,
      labelId
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("partnerId", UserId.encode(event.partnerId)),
          ("labelId", LabelId.encode(event.labelId))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        partnerId: raw |> field("partnerId", UserId.decode),
        labelId: raw |> field("labelId", LabelId.decode)
      };
  };
  include (val EventTypes.makeProcess("PartnerLabel"))(Data);
};

module Contribution = {
  module Data = {
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
  include (val EventTypes.makeProcess("Contribution"))(Data);
};

type t =
  | VentureCreated(VentureCreated.t)
  | PartnerProposed(Partner.Proposal.t)
  | PartnerEndorsed(Partner.Endorsement.t)
  | PartnerAccepted(Partner.Acceptance.t)
  | PartnerLabelProposed(PartnerLabel.Proposal.t)
  | PartnerLabelEndorsed(PartnerLabel.Endorsement.t)
  | PartnerLabelAccepted(PartnerLabel.Acceptance.t)
  | ContributionProposed(Contribution.Proposal.t)
  | ContributionEndorsed(Contribution.Endorsement.t)
  | ContributionAccepted(Contribution.Acceptance.t);

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

let makePartnerLabelProposed = (~partnerId, ~labelId, ~supporterId, ~policy) =>
  PartnerLabelProposed(
    PartnerLabel.Proposal.make(
      ~supporterId,
      ~policy,
      ~data=PartnerLabel.Data.{partnerId, labelId}
    )
  );

let makePartnerLabelEndorsed = (~processId, ~supporterId) =>
  PartnerLabelEndorsed(
    PartnerLabel.Endorsement.make(~processId, ~supporterId)
  );

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
    Contribution.Proposal.make(
      ~supporterId,
      ~policy,
      ~data=
        Contribution.Data.{
          amountInteger,
          amountFraction,
          currency,
          description
        }
    )
  );

let makeContributionEndorsed = (~processId, ~supporterId) =>
  ContributionEndorsed(
    Contribution.Endorsement.make(~processId, ~supporterId)
  );

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | PartnerProposed(event) => Partner.Proposal.encode(event)
  | PartnerEndorsed(event) => Partner.Endorsement.encode(event)
  | PartnerAccepted(event) => Partner.Acceptance.encode(event)
  | PartnerLabelProposed(event) => PartnerLabel.Proposal.encode(event)
  | PartnerLabelEndorsed(event) => PartnerLabel.Endorsement.encode(event)
  | PartnerLabelAccepted(event) => PartnerLabel.Acceptance.encode(event)
  | ContributionProposed(event) => Contribution.Proposal.encode(event)
  | ContributionEndorsed(event) => Contribution.Endorsement.encode(event)
  | ContributionAccepted(event) => Contribution.Acceptance.encode(event);

let isSystemEvent =
  fun
  | PartnerAccepted(_)
  | PartnerLabelAccepted(_)
  | ContributionAccepted(_) => true
  | _ => false;

exception UnknownEvent(Js.Json.t);

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "VentureCreated" => VentureCreated(VentureCreated.decode(raw))
  | "PartnerProposed" => PartnerProposed(Partner.Proposal.decode(raw))
  | "PartnerEndorsed" => PartnerEndorsed(Partner.Endorsement.decode(raw))
  | "PartnerAccepted" => PartnerAccepted(Partner.Acceptance.decode(raw))
  | "PartnerLabelProposed" =>
    PartnerLabelProposed(PartnerLabel.Proposal.decode(raw))
  | "PartnerLabelEndorsed" =>
    PartnerLabelEndorsed(PartnerLabel.Endorsement.decode(raw))
  | "PartnerLabelAccepted" =>
    PartnerLabelAccepted(PartnerLabel.Acceptance.decode(raw))
  | "ContributionProposed" =>
    ContributionProposed(Contribution.Proposal.decode(raw))
  | "ContributionEndorsed" =>
    ContributionEndorsed(Contribution.Endorsement.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(Contribution.Acceptance.decode(raw))
  | _ => raise(UnknownEvent(raw))
  };
};
