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
    distributionGraph: DistributionGraph.t
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
    distributionGraph: DistributionGraph.make(creatorId, initialLabelIds)
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
        (
          "distributionGraph",
          DistributionGraph.encode(event.distributionGraph)
        )
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
      distributionGraph:
        raw |> field("distributionGraph", DistributionGraph.decode)
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

module Custodian = {
  module Data = {
    type t = {partnerId: userId};
    let encode = event =>
      Json.Encode.(object_([("partnerId", UserId.encode(event.partnerId))]));
    let decode = raw =>
      Json.Decode.{partnerId: raw |> field("partnerId", UserId.decode)};
  };
  include (val EventTypes.makeProcess("Custodian"))(Data);
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

module AccountCreation = {
  module Data = {
    type t = {
      accountIndex: int,
      name: string
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("accountIndex", int(event.accountIndex)),
          ("name", string(event.name))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        accountIndex: raw |> field("accountIndex", int),
        name: raw |> field("name", string)
      };
  };
  include (val EventTypes.makeProcess("AccountCreation"))(Data);
};

module PartnerDistribution = {
  module Data = {
    type t = DistributionGraph.PartnerDistribution.t;
    let encode = DistributionGraph.PartnerDistribution.encode;
    let decode = DistributionGraph.PartnerDistribution.decode;
  };
  include (val EventTypes.makeProcess("PartnerDistribution"))(Data);
};

module LabelDistribution = {
  module Data = {
    type t = DistributionGraph.LabelDistribution.t;
    let encode = DistributionGraph.LabelDistribution.encode;
    let decode = DistributionGraph.LabelDistribution.decode;
  };
  include (val EventTypes.makeProcess("LabelDistribution"))(Data);
};

type t =
  | VentureCreated(VentureCreated.t)
  | PartnerProposed(Partner.Proposal.t)
  | PartnerEndorsed(Partner.Endorsement.t)
  | PartnerAccepted(Partner.Acceptance.t)
  | CustodianProposed(Custodian.Proposal.t)
  | CustodianEndorsed(Custodian.Endorsement.t)
  | CustodianAccepted(Custodian.Acceptance.t)
  | AccountCreationProposed(AccountCreation.Proposal.t)
  | AccountCreationEndorsed(AccountCreation.Endorsement.t)
  | AccountCreationAccepted(AccountCreation.Acceptance.t)
  | PartnerLabelProposed(PartnerLabel.Proposal.t)
  | PartnerLabelEndorsed(PartnerLabel.Endorsement.t)
  | PartnerLabelAccepted(PartnerLabel.Acceptance.t)
  | PartnerDistributionProposed(PartnerDistribution.Proposal.t)
  | PartnerDistributionEndorsed(PartnerDistribution.Endorsement.t)
  | PartnerDistributionAccepted(PartnerDistribution.Acceptance.t)
  | LabelDistributionProposed(LabelDistribution.Proposal.t)
  | LabelDistributionEndorsed(LabelDistribution.Endorsement.t)
  | LabelDistributionAccepted(LabelDistribution.Acceptance.t);

let makePartnerProposed = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  PartnerProposed(
    Partner.Proposal.make(
      ~supporterId,
      ~policy,
      Partner.Data.{id: prospectId, pubKey: prospectPubKey}
    )
  );

let makeCustodianProposed = (~supporterId, ~partnerId, ~policy) =>
  CustodianProposed(
    Custodian.Proposal.make(
      ~supporterId,
      ~policy,
      Custodian.Data.{partnerId: partnerId}
    )
  );

let makePartnerEndorsed = (~processId, ~supporterId) =>
  PartnerEndorsed(Partner.Endorsement.make(~processId, ~supporterId));

let makePartnerLabelProposed = (~partnerId, ~labelId, ~supporterId, ~policy) =>
  PartnerLabelProposed(
    PartnerLabel.Proposal.make(
      ~supporterId,
      ~policy,
      PartnerLabel.Data.{partnerId, labelId}
    )
  );

let makePartnerLabelEndorsed = (~processId, ~supporterId) =>
  PartnerLabelEndorsed(
    PartnerLabel.Endorsement.make(~processId, ~supporterId)
  );

let makePartnerDistributionProposed =
    (~supporterId, ~policy, ~labelId, ~distribution) =>
  PartnerDistributionProposed(
    PartnerDistribution.Proposal.make(
      ~supporterId,
      ~policy,
      {labelId, distribution}
    )
  );

let makePartnerDistributionEndorsed = (~processId, ~supporterId) =>
  PartnerDistributionEndorsed(
    PartnerDistribution.Endorsement.make(~processId, ~supporterId)
  );

let makeLabelDistributionProposed =
    (~supporterId, ~policy, ~labelId, ~distribution) =>
  LabelDistributionProposed(
    LabelDistribution.Proposal.make(
      ~supporterId,
      ~policy,
      {labelId, distribution}
    )
  );

let makeLabelDistributionEndorsed = (~processId, ~supporterId) =>
  LabelDistributionEndorsed(
    LabelDistribution.Endorsement.make(~processId, ~supporterId)
  );

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | PartnerProposed(event) => Partner.Proposal.encode(event)
  | PartnerEndorsed(event) => Partner.Endorsement.encode(event)
  | PartnerAccepted(event) => Partner.Acceptance.encode(event)
  | CustodianProposed(event) => Custodian.Proposal.encode(event)
  | CustodianEndorsed(event) => Custodian.Endorsement.encode(event)
  | CustodianAccepted(event) => Custodian.Acceptance.encode(event)
  | AccountCreationProposed(event) => AccountCreation.Proposal.encode(event)
  | AccountCreationEndorsed(event) => AccountCreation.Endorsement.encode(event)
  | AccountCreationAccepted(event) => AccountCreation.Acceptance.encode(event)
  | PartnerLabelProposed(event) => PartnerLabel.Proposal.encode(event)
  | PartnerLabelEndorsed(event) => PartnerLabel.Endorsement.encode(event)
  | PartnerLabelAccepted(event) => PartnerLabel.Acceptance.encode(event)
  | PartnerDistributionProposed(event) =>
    PartnerDistribution.Proposal.encode(event)
  | PartnerDistributionEndorsed(event) =>
    PartnerDistribution.Endorsement.encode(event)
  | PartnerDistributionAccepted(event) =>
    PartnerDistribution.Acceptance.encode(event)
  | LabelDistributionProposed(event) =>
    LabelDistribution.Proposal.encode(event)
  | LabelDistributionEndorsed(event) =>
    LabelDistribution.Endorsement.encode(event)
  | LabelDistributionAccepted(event) =>
    LabelDistribution.Acceptance.encode(event);

let isSystemEvent =
  fun
  | PartnerAccepted(_)
  | CustodianAccepted(_)
  | AccountCreationAccepted(_)
  | PartnerLabelAccepted(_)
  | _ => false;

exception UnknownEvent(Js.Json.t);

let decode = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch type_ {
  | "VentureCreated" => VentureCreated(VentureCreated.decode(raw))
  | "PartnerProposed" => PartnerProposed(Partner.Proposal.decode(raw))
  | "PartnerEndorsed" => PartnerEndorsed(Partner.Endorsement.decode(raw))
  | "PartnerAccepted" => PartnerAccepted(Partner.Acceptance.decode(raw))
  | "CustodianProposed" => CustodianProposed(Custodian.Proposal.decode(raw))
  | "CustodianEndorsed" => CustodianEndorsed(Custodian.Endorsement.decode(raw))
  | "CustodianAccepted" => CustodianAccepted(Custodian.Acceptance.decode(raw))
  | "AccountCreationProposed" =>
    AccountCreationProposed(AccountCreation.Proposal.decode(raw))
  | "AccountCreationEndorsed" =>
    AccountCreationEndorsed(AccountCreation.Endorsement.decode(raw))
  | "AccountCreationAccepted" =>
    AccountCreationAccepted(AccountCreation.Acceptance.decode(raw))
  | "PartnerLabelProposed" =>
    PartnerLabelProposed(PartnerLabel.Proposal.decode(raw))
  | "PartnerLabelEndorsed" =>
    PartnerLabelEndorsed(PartnerLabel.Endorsement.decode(raw))
  | "PartnerLabelAccepted" =>
    PartnerLabelAccepted(PartnerLabel.Acceptance.decode(raw))
  | "PartnerDistributionProposed" =>
    PartnerDistributionProposed(PartnerDistribution.Proposal.decode(raw))
  | "PartnerDistributionEndorsed" =>
    PartnerDistributionEndorsed(PartnerDistribution.Endorsement.decode(raw))
  | "PartnerDistributionAccepted" =>
    PartnerDistributionAccepted(PartnerDistribution.Acceptance.decode(raw))
  | "LabelDistributionProposed" =>
    LabelDistributionProposed(LabelDistribution.Proposal.decode(raw))
  | "LabelDistributionEndorsed" =>
    LabelDistributionEndorsed(LabelDistribution.Endorsement.decode(raw))
  | "LabelDistributionAccepted" =>
    LabelDistributionAccepted(LabelDistribution.Acceptance.decode(raw))
  | _ => raise(UnknownEvent(raw))
  };
};
