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

exception CouldNotLoadVenture;

type t;

let join:
  (Session.Data.t, ~userId: userId, ~ventureId: ventureId) =>
  Js.Promise.t((Index.t, t));

let load: (Session.Data.t, ~ventureId: ventureId) => Js.Promise.t(t);

let getId: t => ventureId;

let getAllEvents: t => list(Event.t);

let getSummary: t => EventLog.summary;

module Cmd: {
  module Create: {
    type result = (Index.t, t);
    let exec:
      (Session.Data.t, ~name: string) => (ventureId, Js.Promise.t(result));
  };
  module SynchronizeLogs: {
    type result =
      | Ok(t, list(EventLog.item))
      | WithConflicts(
          t,
          list(EventLog.item),
          list((EventLog.item, Validation.result)),
        );
    let exec: (list(EventLog.item), t) => Js.Promise.t(result);
  };
  module SynchronizeWallet: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (list(Event.IncomeDetected.t), t) => Js.Promise.t(result);
  };
  module ProposePartner: {
    type result =
      | Ok(t, list(EventLog.item))
      | PartnerAlreadyExists
      | NoUserInfo;
    let exec: (~prospectId: userId, t) => Js.Promise.t(result);
  };
  module RejectPartner: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePartner: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module ProposePartnerRemoval: {
    type result =
      | Ok(t, list(EventLog.item))
      | PartnerDoesNotExist;
    let exec: (~partnerId: userId, t) => Js.Promise.t(result);
  };
  module RejectPartnerRemoval: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePartnerRemoval: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module ExposeIncomeAddress: {
    type result =
      | Ok(string, t, list(EventLog.item));
    let exec: (~accountIdx: accountIdx, t) => Js.Promise.t(result);
  };
  module ProposePayout: {
    type result =
      | Ok(t, list(EventLog.item));
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
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module EndorsePayout: {
    type result =
      | Ok(t, list(EventLog.item));
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
};
