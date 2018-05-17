open PrimitiveTypes;

open Event;

type t;

let make: unit => t;

let systemIssuer: t => Bitcoin.ECPair.t;

let ventureName: t => string;

let currentPolicy: (string, t) => Policy.t;

let currentPartners: t => UserId.set;

let isPartner: (userId, t) => bool;

let custodianAcceptedFor: (userId, t) => option(Custodian.Accepted.t);

let lastRemovalOfPartner: (userId, t) => option(Partner.Removal.Accepted.t);

let lastRemovalOfCustodian:
  (userId, t) => option(Custodian.Removal.Accepted.t);

let lastPartnerAccepted: (userId, t) => Partner.Accepted.t;

let custodianProcessForPartnerProcess: (processId, t) => processId;

let custodianRemovalProcessForPartnerRemovalProcess:
  (processId, t) => option(processId);

let apply: (Event.t, t) => t;
