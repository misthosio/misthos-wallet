module Index: {
  type item = {
    id: string,
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
    | DuplicateApproval
    | PolicyMissmatch
    | PolicyNotFulfilled;
};

exception InvalidEvent(Validation.result);

type t;

let join:
  (Session.Data.t, ~blockstackId: string, ~ventureId: string) =>
  Js.Promise.t((Index.t, t));

let load: (~ventureId: string) => Js.Promise.t(t);

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
    let exec: (Session.Data.t, ~prospectId: string, t) => Js.Promise.t(result);
  };
  module ApproveProspect: {
    type result =
      | Ok(t);
    let exec: (Session.Data.t, ~prospectId: string, t) => Js.Promise.t(result);
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
  module ApproveContribution: {
    type result =
      | Ok(t);
    let exec: (Session.Data.t, ~processId: string, t) => Js.Promise.t(result);
  };
};
