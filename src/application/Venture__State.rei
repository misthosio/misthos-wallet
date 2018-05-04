open PrimitiveTypes;

open Event;

type t;

let make: unit => t;

let systemIssuer: t => Bitcoin.ECPair.t;

let ventureName: t => string;

let currentPolicy: (string, t) => Policy.t;

let isPartner: (userId, t) => bool;

let custodianProcessForPartnerProcess: (processId, t) => processId;

let custodianAcceptedFor: (userId, t) => Custodian.Accepted.t;

let custodianRemovalProcessForPartnerRemovalProcess:
  (processId, t) => processId;

let lastRemovalOfPartner: (userId, t) => option(Partner.Removal.Accepted.t);

let apply: (Event.t, t) => t;
