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
  custodianAccepted: list((userId, Custodian.Accepted.t)),
  partnerRemovals: list((userId, Partner.Removal.Accepted.t)),
};

let make = () => {
  ventureName: "",
  systemIssuer: Bitcoin.ECPair.makeRandom(),
  policies: [],
  partnerIds: [],
  custodianProcesses: [],
  partnerRemovalProcesses: [],
  custodianRemovalProcesses: [],
  custodianAccepted: [],
  partnerRemovals: [],
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

let custodianAcceptedFor = (partnerId, {custodianAccepted}) =>
  custodianAccepted |> List.assoc(partnerId);

let custodianRemovalProcessForPartnerRemovalProcess =
    (processId, {custodianRemovalProcesses, partnerRemovalProcesses}) => {
  let custodianId = partnerRemovalProcesses |> List.assoc(processId);
  custodianRemovalProcesses |> List.assoc(custodianId);
};

let lastRemovalOfPartner = (partnerId, {partnerRemovals}) =>
  try (Some(partnerRemovals |> List.assoc(partnerId))) {
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
  | PartnerAccepted({data: {id}}) => {
      ...state,
      partnerIds: [id, ...state.partnerIds],
    }
  | CustodianProposed({processId, data: {partnerApprovalProcess, partnerId}}) => {
      ...state,
      custodianProcesses: [
        (processId, (partnerId, partnerApprovalProcess)),
        ...state.custodianProcesses,
      ],
    }
  | CustodianAccepted({data: {partnerId}} as event) => {
      ...state,
      custodianAccepted: [(partnerId, event), ...state.custodianAccepted],
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
  | PartnerRemovalAccepted({data: {id}} as event) => {
      ...state,
      partnerIds: state.partnerIds |> List.filter(UserId.neq(id)),
      partnerRemovals: [(id, event), ...state.partnerRemovals],
    }
  | _ => state
  };
