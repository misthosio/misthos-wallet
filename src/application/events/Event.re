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

module Custodian = {
  module Data = {
    type t = {
      partnerId: userId,
      accountIndex: int
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("partnerId", UserId.encode(event.partnerId)),
          ("accountIndex", int(event.accountIndex))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        partnerId: raw |> field("partnerId", UserId.decode),
        accountIndex: raw |> field("accountIndex", int)
      };
  };
  include (val EventTypes.makeProcess("Custodian"))(Data);
};

module CustodianKeyChainUpdated = {
  type t = {
    partnerId: userId,
    keyChain: CustodianKeyChain.public
  };
  let make = (~partnerId, ~keyChain) => {partnerId, keyChain};
  let encode = event =>
    Json.Encode.(
      object_([
        ("partnerId", UserId.encode(event.partnerId)),
        ("keyChain", CustodianKeyChain.encode(event.keyChain))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      partnerId: raw |> field("partnerId", UserId.decode),
      keyChain: raw |> field("keyChain", CustodianKeyChain.decode)
    };
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
  | AccountCreationAccepted(AccountCreation.Acceptance.t);

let makePartnerProposed = (~supporterId, ~prospectId, ~prospectPubKey, ~policy) =>
  PartnerProposed(
    Partner.Proposal.make(
      ~supporterId,
      ~policy,
      Partner.Data.{id: prospectId, pubKey: prospectPubKey}
    )
  );

let makeAccountCreationProposed = (~supporterId, ~name, ~accountIndex, ~policy) =>
  AccountCreationProposed(
    AccountCreation.Proposal.make(
      ~supporterId,
      ~policy,
      AccountCreation.Data.{accountIndex, name}
    )
  );

let makeCustodianProposed = (~supporterId, ~partnerId, ~accountIndex, ~policy) =>
  CustodianProposed(
    Custodian.Proposal.make(
      ~supporterId,
      ~policy,
      Custodian.Data.{partnerId, accountIndex}
    )
  );

let makePartnerEndorsed = (~processId, ~supporterId) =>
  PartnerEndorsed(Partner.Endorsement.make(~processId, ~supporterId));

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
  | AccountCreationAccepted(event) => AccountCreation.Acceptance.encode(event);

let isSystemEvent =
  fun
  | PartnerAccepted(_)
  | AccountCreationAccepted(_)
  | CustodianAccepted(_) => true
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
  | _ => raise(UnknownEvent(raw))
  };
};
