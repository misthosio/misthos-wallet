open PrimitiveTypes;

open Event;

type t = {
  ventureName: string,
  systemIssuer: Bitcoin.ECPair.t,
  policies: list((string, Policy.t)),
  partnerIds: list(userId),
  custodianProcesses: list((processId, (userId, processId))),
  partnerRemovalProcesses: list((processId, userId)),
  custodianRemovalProcesses: list((userId, processId)),
  completedPartnerRemovalProcesses: list((userId, processId)),
};

let make = () => {
  ventureName: "",
  systemIssuer: Bitcoin.ECPair.makeRandom(),
  policies: [],
  partnerIds: [],
  custodianProcesses: [],
  partnerRemovalProcesses: [],
  custodianRemovalProcesses: [],
  completedPartnerRemovalProcesses: [],
};

let systemIssuer = ({systemIssuer}) => systemIssuer;

let ventureName = ({ventureName}) => ventureName;

let currentPolicy = (processName, {policies}) =>
  policies |> List.assoc(processName);

let isPartner = (id, {partnerIds}) => partnerIds |> List.mem(id);

let custodianProcessForPartnerProcess = (processId, {custodianProcesses}) =>
  custodianProcesses
  |> List.find(((_, (_, partnerProcess))) =>
       ProcessId.eq(processId, partnerProcess)
     )
  |> fst;

let custodianProcessForPartner = (partnerId, {custodianProcesses}) =>
  custodianProcesses
  |> List.find(((_, (custodianId, _))) =>
       UserId.eq(partnerId, custodianId)
     )
  |> fst;

let custodianRemovalProcessForPartnerRemovalProcess =
    (processId, {custodianRemovalProcesses, partnerRemovalProcesses}) => {
  let custodianId = partnerRemovalProcesses |> List.assoc(processId);
  custodianRemovalProcesses |> List.assoc(custodianId);
};

let lastRemovalProcessOfPartner =
    (partnerId, {completedPartnerRemovalProcesses}) =>
  try (Some(completedPartnerRemovalProcesses |> List.assoc(partnerId))) {
  | Not_found => None
  };

let apply = (event, state) =>
  switch (event) {
  | VentureCreated({ventureName, metaPolicy, systemIssuer}) => {
      ...state,
      ventureName,
      systemIssuer,
      policies: [
        (Partner.Removal.processName, Policy.UnanimousMinusOne),
        (Custodian.Removal.processName, Policy.UnanimousMinusOne),
        ...[
             Partner.processName,
             AccountCreation.processName,
             Custodian.processName,
             Payout.processName,
           ]
           |> List.map(n => (n, metaPolicy)),
      ],
    }
  | CustodianProposed({processId, data: {partnerApprovalProcess, partnerId}}) => {
      ...state,
      custodianProcesses: [
        (processId, (partnerId, partnerApprovalProcess)),
        ...state.custodianProcesses,
      ],
    }
  | PartnerRemovalProposed({processId, data: {id}}) => {
      ...state,
      partnerRemovalProcesses: [
        (processId, id),
        ...state.partnerRemovalProcesses,
      ],
    }
  | CustodianRemovalProposed({processId, data: {custodianId}}) => {
      ...state,
      custodianRemovalProcesses: [
        (custodianId, processId),
        ...state.custodianRemovalProcesses,
      ],
    }
  | PartnerRemovalAccepted({processId, data: {id}}) => {
      ...state,
      completedPartnerRemovalProcesses: [
        (id, processId),
        ...state.completedPartnerRemovalProcesses,
      ],
    }
  | _ => state
  };
