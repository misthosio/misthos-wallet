module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type t;

let load: (~projectId: string) => Js.Promise.t(t);

let getId: t => string;

let getViewModel: t => ViewModel.t;

let getMemberHistoryUrls: (Session.Data.t, t) => Js.Promise.t(array(string));

module Cmd: {
  module Create: {
    type result = (Index.t, t);
    let exec: (Session.Data.t, ~name: string) => Js.Promise.t(result);
  };
  module SuggestCandidate: {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec:
      (Session.Data.t, ~candidateId: string, t) => Js.Promise.t(result);
  };
};
