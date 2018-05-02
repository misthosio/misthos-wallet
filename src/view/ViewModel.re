open PrimitiveTypes;

open WalletTypes;

type partner = {userId};

type prospect = {
  processId,
  userId,
  endorsedBy: list(userId),
};

type payoutStatus =
  | PayoutPending
  | PayoutCompleted(string)
  | PayoutFailed(string);

type payout = {
  processId,
  payoutTx: PayoutTransaction.t,
  endorsedBy: list(userId),
  status: payoutStatus,
};

type t = {
  ventureId,
  name: string,
  partners: list(partner),
  prospects: list(prospect),
  removalProspects: list(prospect),
  metaPolicy: Policy.t,
  partnerPolicy: Policy.t,
  incomeAddresses: list((accountIdx, list(string))),
  payouts: list(payout),
};

let make = () => {
  name: "",
  ventureId: VentureId.fromString(""),
  partners: [],
  prospects: [],
  removalProspects: [],
  metaPolicy: Policy.unanimous,
  partnerPolicy: Policy.unanimous,
  incomeAddresses: [],
  payouts: [],
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({ventureName, metaPolicy, ventureId}) => {
      ...state,
      ventureId,
      name: ventureName,
      metaPolicy,
      partnerPolicy: metaPolicy,
    }
  | PartnerProposed({processId, supporterId, data}) => {
      ...state,
      prospects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.prospects,
      ],
    }
  | PartnerEndorsed({processId, supporterId}) => {
      ...state,
      prospects:
        state.prospects
        |> List.map((p: prospect) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           ),
    }
  | PartnerAccepted({data}) => {
      ...state,
      partners: [{userId: data.id}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, data.id)),
    }
  | PartnerRemovalProposed({processId, supporterId, data}) => {
      ...state,
      removalProspects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.removalProspects,
      ],
    }
  | PartnerRemovalEndorsed({processId, supporterId}) => {
      ...state,
      removalProspects:
        state.removalProspects
        |> List.map((p: prospect) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           ),
    }
  | PartnerRemovalAccepted({processId, data: {id}}) => {
      ...state,
      partners:
        state.partners
        |> List.filter((p: partner) => UserId.neq(p.userId, id)),
      removalProspects:
        state.removalProspects
        |> List.filter((p: prospect) =>
             ProcessId.neq(p.processId, processId)
           ),
    }
  | AccountCreationAccepted({data}) => {
      ...state,
      incomeAddresses: [(data.accountIdx, [])],
    }
  | IncomeAddressExposed({address, coordinates}) =>
    let accountIdx =
      coordinates |> AccountKeyChain.Address.Coordinates.accountIdx;
    {
      ...state,
      incomeAddresses: [
        (
          accountIdx,
          [address, ...state.incomeAddresses |> List.assoc(accountIdx)],
        ),
        ...state.incomeAddresses,
      ],
    };
  | PayoutProposed({processId, supporterId, data}) => {
      ...state,
      payouts: [
        {
          processId,
          payoutTx: data.payoutTx,
          endorsedBy: [supporterId],
          status: PayoutPending,
        },
        ...state.payouts,
      ],
    }
  | PayoutEndorsed({processId, supporterId}) => {
      ...state,
      payouts:
        state.payouts
        |> List.map((p: payout) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, endorsedBy: [supporterId, ...p.endorsedBy]} : p
           ),
    }
  | PayoutBroadcast({processId, transactionId}) => {
      ...state,
      payouts:
        state.payouts
        |> List.map((p: payout) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, status: PayoutCompleted(transactionId)} : p
           ),
    }
  | PayoutBroadcastFailed({processId, errorMessage}) => {
      ...state,
      payouts:
        state.payouts
        |> List.map((p: payout) =>
             ProcessId.eq(p.processId, processId) ?
               {...p, status: PayoutFailed(errorMessage)} : p
           ),
    }
  | _ => state
  };

let init = List.fold_left((m, e) => m |> apply(e), make());

let applyAll = (events, model) =>
  events |> List.fold_left((m, e) => m |> apply(e), model);

let partners = state => state.partners;

let prospects = state => state.prospects;

let removalProspects = state => state.removalProspects;

let ventureName = state => state.name;

let incomeAddresses = state =>
  state.incomeAddresses |> List.assoc(AccountIndex.default);

let payouts = state => state.payouts;
