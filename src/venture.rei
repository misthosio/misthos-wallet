module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type t;

let load: (~ventureId: string) => Js.Promise.t(t);

let getId: t => string;

let getViewModel: t => ViewModel.t;

let getPartnerHistoryUrls: (Session.Data.t, t) => Js.Promise.t(array(string));

module Cmd: {
  module Create: {
    type result = (Index.t, t);
    let exec: (Session.Data.t, ~name: string) => Js.Promise.t(result);
  };
  module SuggestProspect: {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec: (Session.Data.t, ~prospectId: string, t) => Js.Promise.t(result);
  };
  module Synchronize: {
    type result =
      | Ok(t)
      | Error(EventLog.item, ValidationState.validation);
    let exec: (list(EventLog.t), t) => Js.Promise.t(result);
  };
};
