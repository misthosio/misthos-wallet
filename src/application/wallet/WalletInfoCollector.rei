open PrimitiveTypes;

open WalletTypes;

type addressStatus =
  | Accessible
  | AtRisk
  | OutdatedCustodians
  | TemporarilyInaccessible
  | Inaccessible;
type addressType =
  | Income(userId)
  | Change;

type addressInfo = {
  addressType,
  custodians: UserId.set,
  address: string,
  nCoSigners: int,
  addressStatus,
  balance: BTC.t,
};

type t;

let make: unit => t;

let addressInfos: (accountIdx, t) => list(addressInfo);
let collidingProcesses: (accountIdx, processId, t) => ProcessId.set;

let exposedCoordinates: t => list(Address.Coordinates.t);

let accountKeyChains: t => AccountKeyChain.Collection.t;

let network: t => Network.t;

let apply: (Event.t, t) => t;

let totalUnusedBTC: (accountIdx, t) => BTC.t;

let totalReservedBTC: (accountIdx, t) => BTC.t;

let currentKeyChainIdent:
  (accountIdx, userId, t) => AccountKeyChain.Identifier.t;

let currentKeyChain: (accountIdx, userId, t) => AccountKeyChain.t;

let currentSpendableInputs: (accountIdx, t) => Network.inputSet;
let oldSpendableInputs: (accountIdx, t) => Network.inputSet;
let temporarilyInaccessibleInputs: t => Network.inputSet;
let unlockedInputs: (accountIdx, t) => Network.inputSet;

let inputsFor: (accountIdx, addressInfo, t) => list(Network.txInput);

let nextChangeAddress: (accountIdx, userId, t) => Address.t;
let fakeChangeAddress: (accountIdx, userId, t) => Address.t;
