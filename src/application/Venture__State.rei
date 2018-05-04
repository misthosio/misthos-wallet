open PrimitiveTypes;

open Event;

type t;

let make: unit => t;

let systemIssuer: t => Bitcoin.ECPair.t;

let ventureName: t => string;

let currentPolicy: (string, t) => Policy.t;

let isPartner: (userId, t) => bool;

let custodianAcceptedFor: (userId, t) => Custodian.Accepted.t;

let lastRemovalOfPartner: (userId, t) => option(Partner.Removal.Accepted.t);

let custodianProcessForPartnerProcess: (processId, t) => processId;

let custodianRemovalProcessForPartnerRemovalProcess:
  (processId, t) => processId;

let apply: (Event.t, t) => t;
