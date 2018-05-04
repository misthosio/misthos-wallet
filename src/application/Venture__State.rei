open PrimitiveTypes;

type t;

let make: unit => t;

let systemIssuer: t => Bitcoin.ECPair.t;

let ventureName: t => string;

let currentPolicy: (string, t) => Policy.t;

let isPartner: (userId, t) => bool;

let custodianProcessForPartnerProcess: (processId, t) => processId;

let custodianProcessForPartner: (userId, t) => processId;

let custodianRemovalProcessForPartnerRemovalProcess:
  (processId, t) => processId;

let lastRemovalProcessOfPartner: (userId, t) => option(processId);

let apply: (Event.t, t) => t;
