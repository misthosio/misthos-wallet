open PrimitiveTypes;

open WalletTypes;

type addressStatus =
  | Accessible
  | AtRisk
  | OutdatedCustodians;
type addressType =
  | Income
  | Change;

type addressInfo = {
  addressType,
  custodians: UserId.set,
  address: string,
  nCoSigners: int,
  balance: BTC.t,
  addressStatus,
};

type t;

let make: unit => t;

let addressInfos: t => list(addressInfo);

let collidingProcesses: (processId, t) => ProcessId.set;

let exposedCoordinates: t => list(Address.Coordinates.t);

let accountKeyChains: t => AccountKeyChain.Collection.t;

let network: t => Network.t;

let apply: (Event.t, t) => t;

let totalUnusedBTC: t => BTC.t;

let totalReservedBTC: t => BTC.t;

let currentKeyChainIdent:
  (accountIdx, userId, t) => AccountKeyChain.Identifier.t;

let currentKeyChain: (accountIdx, userId, t) => AccountKeyChain.t;

let nonReservedOldInputs: (accountIdx, userId, t) => Network.inputSet;

let unusedInputs: t => Network.inputSet;

let nextChangeAddress: (accountIdx, userId, t) => Address.t;
let fakeChangeAddress: (accountIdx, userId, t) => Address.t;
