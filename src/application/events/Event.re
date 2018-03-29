open PrimitiveTypes;

open WalletTypes;

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
      accountIndex: accountIdx,
      name: string
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("accountIndex", AccountIndex.encode(event.accountIndex)),
          ("name", string(event.name))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        accountIndex: raw |> field("accountIndex", AccountIndex.decode),
        name: raw |> field("name", string)
      };
  };
  include (val EventTypes.makeProcess("AccountCreation"))(Data);
};

module Custodian = {
  module Data = {
    type t = {
      partnerId: userId,
      accountIndex: accountIdx
    };
    let encode = event =>
      Json.Encode.(
        object_([
          ("partnerId", UserId.encode(event.partnerId)),
          ("accountIndex", AccountIndex.encode(event.accountIndex))
        ])
      );
    let decode = raw =>
      Json.Decode.{
        partnerId: raw |> field("partnerId", UserId.decode),
        accountIndex: raw |> field("accountIndex", AccountIndex.decode)
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
        ("type", string("CustodianKeyChainUpdated")),
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

module AccountKeyChainUpdated = {
  type t = {
    accountIndex: accountIdx,
    keyChainIndex: accountKeyChainIdx,
    keyChain: AccountKeyChain.t
  };
  let make = (~accountIndex, ~keyChainIndex, ~keyChain) => {
    accountIndex,
    keyChainIndex,
    keyChain
  };
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("AccountKeyChainUpdated")),
        ("accountIndex", AccountIndex.encode(event.accountIndex)),
        ("keyChainIndex", AccountKeyChainIndex.encode(event.keyChainIndex)),
        ("keyChain", AccountKeyChain.encode(event.keyChain))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      accountIndex: raw |> field("accountIndex", AccountIndex.decode),
      keyChainIndex: raw |> field("keyChainIndex", AccountKeyChainIndex.decode),
      keyChain: raw |> field("keyChain", AccountKeyChain.decode)
    };
};

module IncomeAddressExposed = {
  type t = {
    coordinates: AddressCoordinates.t,
    address: string
  };
  let make = (~coordinates, ~address) => {coordinates, address};
  let encode = event =>
    Json.Encode.(
      object_([
        ("type", string("IncomeAddressExposed")),
        ("coordinates", AddressCoordinates.encode(event.coordinates)),
        ("address", string(event.address))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      coordinates: raw |> field("coordinates", AddressCoordinates.decode),
      address: raw |> field("address", string)
    };
};

type t =
  | VentureCreated(VentureCreated.t)
  | PartnerProposed(Partner.Proposal.t)
  | PartnerEndorsed(Partner.Endorsement.t)
  | PartnerAccepted(Partner.Acceptance.t)
  | AccountCreationProposed(AccountCreation.Proposal.t)
  | AccountCreationEndorsed(AccountCreation.Endorsement.t)
  | AccountCreationAccepted(AccountCreation.Acceptance.t)
  | CustodianProposed(Custodian.Proposal.t)
  | CustodianEndorsed(Custodian.Endorsement.t)
  | CustodianAccepted(Custodian.Acceptance.t)
  | CustodianKeyChainUpdated(CustodianKeyChainUpdated.t)
  | AccountKeyChainUpdated(AccountKeyChainUpdated.t)
  | IncomeAddressExposed(IncomeAddressExposed.t);

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
  | AccountCreationAccepted(event) => AccountCreation.Acceptance.encode(event)
  | CustodianKeyChainUpdated(event) => CustodianKeyChainUpdated.encode(event)
  | AccountKeyChainUpdated(event) => AccountKeyChainUpdated.encode(event)
  | IncomeAddressExposed(event) => IncomeAddressExposed.encode(event);

let isSystemEvent =
  fun
  | PartnerAccepted(_)
  | AccountCreationAccepted(_)
  | CustodianAccepted(_)
  | AccountKeyChainUpdated(_) => true
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
  | "CustodianKeyChainUpdated" =>
    CustodianKeyChainUpdated(CustodianKeyChainUpdated.decode(raw))
  | "AccountKeyChainUpdated" =>
    AccountKeyChainUpdated(AccountKeyChainUpdated.decode(raw))
  | "IncomeAddressExposed" =>
    IncomeAddressExposed(IncomeAddressExposed.decode(raw))
  | _ => raise(UnknownEvent(raw))
  };
};
