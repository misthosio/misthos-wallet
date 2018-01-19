module DealCreated = {
  type t = {
    dealId: string,
    dealName: string,
    creatorId: string,
    creatorPubKey: string,
    metaPolicy: Policy.t,
    systemIssuer: Bitcoin.ECPair.t
  };
  let make = (~dealName, ~creatorId, ~creatorPubKey, ~metaPolicy) => {
    dealId: Uuid.v4(),
    dealName,
    creatorId,
    creatorPubKey,
    metaPolicy,
    systemIssuer: Bitcoin.ECPair.makeRandom()
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("DealCreated")),
        ("dealId", string(event.dealId)),
        ("dealName", string(event.dealName)),
        ("creatorId", string(event.creatorId)),
        ("creatorPubKey", string(event.creatorPubKey)),
        ("metaPolicy", Policy.encode(event.metaPolicy)),
        ("systemIssuer", string(Bitcoin.ECPair.toWIF(event.systemIssuer)))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      dealId: raw |> field("dealId", string),
      dealName: raw |> field("dealName", string),
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
    prospectPubKey: string
  };
  let make = (~supporterId, ~prospectId, ~prospectPubKey) => {
    processId: Uuid.v4(),
    supporterId,
    prospectId,
    prospectPubKey
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("ProspectSuggested")),
        ("processId", string(event.processId)),
        ("supporterId", string(event.supporterId)),
        ("prospectId", string(event.prospectId)),
        ("prospectPubKey", string(event.prospectPubKey))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      processId: raw |> field("processId", string),
      supporterId: raw |> field("supporterId", string),
      prospectId: raw |> field("prospectId", string),
      prospectPubKey: raw |> field("prospectPubKey", string)
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
  | DealCreated(DealCreated.t)
  | ProspectSuggested(ProspectSuggested.t)
  | ProspectApproved(ProspectApproved.t)
  | PartnerAdded(PartnerAdded.t)
  | ContributionSubmitted(ContributionSubmitted.t)
  | ContributionApproved(ContributionApproved.t)
  | ContributionAccepted(ContributionAccepted.t);

let makeProspectSuggested = (~supporterId, ~prospectId, ~prospectPubKey) =>
  ProspectSuggested(
    ProspectSuggested.make(~supporterId, ~prospectId, ~prospectPubKey)
  );

let encode =
  fun
  | DealCreated(event) => DealCreated.encode(event)
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
  | "DealCreated" => DealCreated(DealCreated.decode(raw))
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
