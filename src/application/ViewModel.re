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
  name: string,
  partners: list(partner),
  prospects: list(prospect),
  metaPolicy: Policy.t,
  partnerPolicy: Policy.t,
  incomeAddresses: list((accountIdx, list(string))),
  payouts: list(payout),
};

let make = () => {
  name: "",
  partners: [],
  prospects: [],
  metaPolicy: Policy.absolute,
  partnerPolicy: Policy.absolute,
  incomeAddresses: [],
  payouts: [],
};

let apply = (event: Event.t, state) =>
  switch (event) {
  | VentureCreated({ventureName, metaPolicy}) => {
      ...state,
      name: ventureName,
      metaPolicy,
      partnerPolicy: metaPolicy,
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
  | PartnerProposed({processId, supporterId, data}) => {
      ...state,
      prospects: [
        {processId, userId: data.id, endorsedBy: [supporterId]},
        ...state.prospects,
      ],
    }
  | PartnerAccepted({data}) => {
      ...state,
      partners: [{userId: data.id}, ...state.partners],
      prospects:
        state.prospects |> List.filter(p => UserId.neq(p.userId, data.id)),
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

let partners = state => state.partners;

let prospects = state => state.prospects;

let ventureName = state => state.name;

let incomeAddresses = state =>
  state.incomeAddresses |> List.assoc(AccountIndex.default);

let pendingPayouts = state =>
  state.payouts
  |> List.filter(p =>
       switch (p.status) {
       | PayoutPending => true
       | _ => false
       }
     );

let completedPayouts = state =>
  state.payouts
  |> List.filter(p =>
       switch (p.status) {
       | PayoutPending => false
       | _ => true
       }
     );
