open PrimitiveTypes;

open WalletTypes;

type partner = {userId};

type prospect = {
  processId,
  userId,
  endorsedBy: list(userId),
};

type t = {
  name: string,
  partners: list(partner),
  prospects: list(prospect),
  metaPolicy: Policy.t,
  partnerPolicy: Policy.t,
  incomeAddresses: list((WalletTypes.accountIdx, list(string))),
};

let make = () => {
  name: "",
  partners: [],
  prospects: [],
  metaPolicy: Policy.absolute,
  partnerPolicy: Policy.absolute,
  incomeAddresses: [],
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
  | AccountKeyChainUpdated({keyChain}) => {
      ...state,
      incomeAddresses: [(keyChain.accountIdx, [])],
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
  | _ => state
  };

let partners = state => state.partners;

let prospects = state => state.prospects;

let ventureName = state => state.name;

let incomeAddresses = state =>
  state.incomeAddresses |> List.assoc(AccountIndex.default);
