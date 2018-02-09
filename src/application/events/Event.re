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

module PartnerData = {
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

module PartnerProposed =
  (val EventMaker.makeProposalEvent("PartnerProposed"))(PartnerData);

module ProspectEndorsed = (
  val EventMaker.makeEndorseEvent("ProspectEndorsed")
);

module PartnerAdded = {
  type t = {
    processId,
    partnerId: userId,
    partnerPubKey: string
  };
  let make = (~processId, ~partnerId, ~partnerPubKey) => {
    processId,
    partnerId,
    partnerPubKey
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("PartnerAdded")),
        ("processId", ProcessId.encode(event.processId)),
        ("partnerId", UserId.encode(event.partnerId)),
        ("partnerPubKey", string(event.partnerPubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", ProcessId.decode),
      partnerId: raw |> field("partnerId", UserId.decode),
      partnerPubKey: raw |> field("partnerPubKey", string)
    };
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
  val EventMaker.makeEndorseEvent("PartnerLabelEndorsed")
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

module ContributionEndorsed = (
  val EventMaker.makeEndorseEvent("ContributionEndorsed")
);

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
  | PartnerProposed(PartnerProposed.t)
  | ProspectEndorsed(ProspectEndorsed.t)
  | PartnerAdded(PartnerAdded.t)
  | PartnerLabelSuggested(PartnerLabelSuggested.t)
  | PartnerLabelEndorsed(PartnerLabelEndorsed.t)
  | PartnerLabelAccepted(PartnerLabelAccepted.t)
  | ContributionSubmitted(ContributionSubmitted.t)
  | ContributionEndorsed(ContributionEndorsed.t)
  | ContributionAccepted(ContributionAccepted.t);

let makePartnerProposed = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  PartnerProposed(
    PartnerProposed.make(
      ~supporterId,
      ~policy,
      ~data=PartnerData.{id: prospectId, pubKey: prospectPubKey}
    )
  );

let makeProspectEndorsed = (~processId, ~supporterId) =>
  ProspectEndorsed(ProspectEndorsed.make(~processId, ~supporterId));

let makePartnerLabelSuggested = (~partnerId, ~labelId, ~supporterId, ~policy) =>
  PartnerLabelSuggested(
    PartnerLabelSuggested.make(~partnerId, ~labelId, ~supporterId, ~policy)
  );

let makePartnerLabelEndorsed = (~processId, ~supporterId) =>
  PartnerLabelEndorsed(PartnerLabelEndorsed.make(~processId, ~supporterId));

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

let makeContributionEndorsed = (~processId, ~supporterId) =>
  ContributionEndorsed(ContributionEndorsed.make(~processId, ~supporterId));

let encode =
  fun
  | VentureCreated(event) => VentureCreated.encode(event)
  | PartnerProposed(event) => PartnerProposed.encode(event)
  | ProspectEndorsed(event) => ProspectEndorsed.encode(event)
  | PartnerAdded(event) => PartnerAdded.encode(event)
  | PartnerLabelSuggested(event) => PartnerLabelSuggested.encode(event)
  | PartnerLabelEndorsed(event) => PartnerLabelEndorsed.encode(event)
  | PartnerLabelAccepted(event) => PartnerLabelAccepted.encode(event)
  | ContributionSubmitted(event) => ContributionSubmitted.encode(event)
  | ContributionEndorsed(event) => ContributionEndorsed.encode(event)
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
  | "PartnerProposed" => PartnerProposed(PartnerProposed.decode(raw))
  | "ProspectEndorsed" => ProspectEndorsed(ProspectEndorsed.decode(raw))
  | "PartnerAdded" => PartnerAdded(PartnerAdded.decode(raw))
  | "PartnerLabelSuggested" =>
    PartnerLabelSuggested(PartnerLabelSuggested.decode(raw))
  | "PartnerLabelEndorsed" =>
    PartnerLabelEndorsed(PartnerLabelEndorsed.decode(raw))
  | "PartnerLabelAccepted" =>
    PartnerLabelAccepted(PartnerLabelAccepted.decode(raw))
  | "ContributionSubmitted" =>
    ContributionSubmitted(ContributionSubmitted.decode(raw))
  | "ContributionEndorsed" =>
    ContributionEndorsed(ContributionEndorsed.decode(raw))
  | "ContributionAccepted" =>
    ContributionAccepted(ContributionAccepted.decode(raw))
  | _ => raise(Not_found)
  };
};
