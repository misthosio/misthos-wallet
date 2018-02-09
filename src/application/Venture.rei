open PrimitiveTypes;

module Index: {
  type item = {
    id: ventureId,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

module Validation: {
  type result =
    | Ok
    | InvalidIssuer
    | UnknownProcessId
    | BadData
    | DuplicateEndorsement
    | PolicyMissmatch
    | PolicyNotFulfilled;
};

exception InvalidEvent(Validation.result);

type t;

let join:
  (Session.Data.t, ~userId: string, ~ventureId: string) =>
  Js.Promise.t((Index.t, t));

let load: (~ventureId: ventureId) => Js.Promise.t(t);

let getId: t => string;

let getSummary: t => EventLog.summary;

/* TODO remove dependency. Move into view Folder */
let getViewModel: t => ViewModel.t;

let getPartnerHistoryUrls: (Session.Data.t, t) => Js.Promise.t(array(string));

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
  module SuggestProspect: {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec: (Session.Data.t, ~prospectId: userId, t) => Js.Promise.t(result);
  };
  module EndorseProspect: {
    type result =
      | Ok(t);
    let exec:
      (Session.Data.t, ~processId: processId, t) => Js.Promise.t(result);
  };
  module SuggestPartnerLabel: {
    type result =
      | Ok(t);
    let exec:
      (Session.Data.t, ~partnerId: userId, ~labelId: labelId, t) =>
      Js.Promise.t(result);
  };
  module EndorsePartnerLabel: {
    type result =
      | Ok(t);
    let exec:
      (Session.Data.t, ~processId: processId, t) => Js.Promise.t(result);
  };
  module SubmitContribution: {
    type result =
      | Ok(t);
    let exec:
      (
        Session.Data.t,
        ~amountInteger: int,
        ~amountFraction: int,
        ~currency: string,
        ~description: string,
        t
      ) =>
      Js.Promise.t(result);
  };
  module EndorseContribution: {
    type result =
      | Ok(t);
    let exec:
      (Session.Data.t, ~processId: processId, t) => Js.Promise.t(result);
  };
};
