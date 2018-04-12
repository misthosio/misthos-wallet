open PrimitiveTypes;

open WalletTypes;

module Index: {
  type item = {
    id: ventureId,
    name: string,
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

module Validation: {
  type result =
    | Ok
    | InvalidIssuer
    | UnknownProcessId
    | BadData(string)
    | DuplicateEndorsement
    | PolicyMissmatch
    | PolicyNotFulfilled
    | DependencyNotMet;
};

exception InvalidEvent(Validation.result);

exception CouldNotLoadVenture;

type t;

let join:
  (Session.Data.t, ~userId: string, ~ventureId: string) =>
  Js.Promise.t((Index.t, t));

let load: (Session.Data.t, ~ventureId: ventureId) => Js.Promise.t(t);

let getId: t => string;

let getSummary: t => EventLog.summary;

/* TODO remove dependency. Move into view Folder */
let getViewModel: t => ViewModel.t;

let getPartnerHistoryUrls: t => Js.Promise.t(array(string));

module Wallet: {
  type balance = {
    total: BTC.t,
    reserved: BTC.t,
  };
  let balance: t => Js.Promise.t(balance);
};

module Cmd: {
  module Create: {
    type result = (Index.t, t);
    let exec: (Session.Data.t, ~name: string) => Js.Promise.t(result);
  };
  module Synchronize: {
    type result =
      | Ok(t)
      | Error(t, EventLog.item, Validation.result);
    let exec: (list(EventLog.t), t) => Js.Promise.t(result);
  };
  module ProposePartner: {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec: (~prospectId: userId, t) => Js.Promise.t(result);
  };
  module EndorsePartner: {
    type result =
      | Ok(t);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  module ExposeIncomeAddress: {
    type result =
      | Ok(string, t);
    let exec: (~accountIdx: accountIdx, t) => Js.Promise.t(result);
  };
  module ProposePayout: {
    type result =
      | Ok(t);
    let exec:
      (
        ~accountIdx: accountIdx,
        ~destinations: list((string, BTC.t)),
        ~fee: BTC.t,
        t
      ) =>
      Js.Promise.t(result);
  };
  module EndorsePayout: {
    type result =
      | Ok(t);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
};
