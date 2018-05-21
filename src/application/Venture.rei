open PrimitiveTypes;

open WalletTypes;

module Index: {
  type item = {
    id: ventureId,
    name: string,
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module Validation: {
  type result =
    | Ok
    | Ignore
    | InvalidIssuer
    | UnknownProcessId
    | AlreadyEndorsed
    | PolicyMissmatch
    | PolicyNotFulfilled
    | DependencyNotMet
    | BadData(string);
  let resultToString: result => string;
};

exception InvalidEvent(Validation.result);

exception NotPersistingNewEvents;

type t;

type loadResult =
  | Ok(t, array(EventLog.item))
  | CouldNotLoad(Js.Promise.error);

let load:
  (~persist: bool=?, Session.Data.t, ~ventureId: ventureId) =>
  Js.Promise.t(loadResult);

type joinResult =
  | AlreadyLoaded(Index.t, t, array(EventLog.item))
  | Joined(Index.t, t)
  | CouldNotJoin(Js.Promise.error);

let join:
  (Session.Data.t, ~userId: userId, ~ventureId: ventureId) =>
  Js.Promise.t(joinResult);

let getId: t => ventureId;

let getEventLog: t => EventLog.t;

let getSummary: t => EventLog.summary;

module Cmd: {
  module Create: {
    type result =
      | Ok(Index.t, t)
      | CouldNotPersist(Js.Promise.error);
    let exec:
      (Session.Data.t, ~name: string) => (ventureId, Js.Promise.t(result));
  };
  module SynchronizeLogs: {
    type result =
      | Ok(t, array(EventLog.item))
      | WithConflicts(
          t,
          array(EventLog.item),
          array((EventLog.item, Validation.result)),
        )
      | CouldNotPersist(Js.Promise.error);
    let exec: (array(EventLog.item), t) => Js.Promise.t(result);
  };
  module SynchronizeWallet: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec:
      (
        list(Event.IncomeDetected.t),
        list(Event.Transaction.Confirmed.t),
        t
      ) =>
      Js.Promise.t(result);
  };
  module ProposePartner: {
    type result =
      | Ok(t, array(EventLog.item))
      | PartnerAlreadyExists
      | NoUserInfo
      | CouldNotPersist(Js.Promise.error);
    let exec: (~prospectId: userId, t) => Js.Promise.t(result);
  };
  module RejectPartner: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePartner: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module ProposePartnerRemoval: {
    type result =
      | Ok(t, array(EventLog.item))
      | PartnerDoesNotExist
      | CouldNotPersist(Js.Promise.error);
    let exec: (~partnerId: userId, t) => Js.Promise.t(result);
  };
  module RejectPartnerRemoval: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePartnerRemoval: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module ExposeIncomeAddress: {
    type result =
      | Ok(string, t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~accountIdx: accountIdx, t) => Js.Promise.t(result);
  };
  module ProposePayout: {
    type result =
      | Ok(t, array(EventLog.item))
      | NotEnoughFunds
      | CouldNotPersist(Js.Promise.error);
    let exec:
      (
        ~accountIdx: accountIdx,
        ~destinations: list((string, BTC.t)),
        ~fee: BTC.t,
        t
      ) =>
      Js.Promise.t(result);
  };
  module RejectPayout: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePayout: {
    type result =
      | Ok(t, array(EventLog.item))
      | CouldNotPersist(Js.Promise.error);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
};
