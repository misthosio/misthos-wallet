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
    | PolicyNotFulfilled
    | DependencyNotMet;
};

exception InvalidEvent(Validation.result);

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

module Cmd: {
  module Create: {
    type result = (Index.t, t);
    let exec:
      (Session.Data.t, ~name: string, ~initialLabelIds: list(LabelId.t)) =>
      Js.Promise.t(result);
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
  module ProposePartnerLabel: {
    type result =
      | Ok(t);
    let exec:
      (~partnerId: userId, ~labelId: labelId, t) => Js.Promise.t(result);
  };
  module EndorsePartnerLabel: {
    type result =
      | Ok(t);
    let exec: (~processId: processId, t) => Js.Promise.t(result);
  };
  /* module ProposeOutputDistributionNode: { */
  /*   type result = */
  /*     | Ok(t); */
  /*   let exec: */
  /*     ( */
  /*       Session.Data.t, */
  /*       ~labelId: labelId, */
  /*       ~distribution: list((userId, int)), */
  /*       t */
  /*     ) => */
  /*     Js.Promise.t(result); */
  /* }; */
  /* module ProposeInternalDistributionNode: { */
  /*   type result = */
  /*     | Ok(t); */
  /*   let exec: */
  /*     ( */
  /*       Session.Data.t, */
  /*       ~labelId: labelId, */
  /*       ~distribution: list((labelId, int)), */
  /*       t */
  /*     ) => */
  /*     Js.Promise.t(result); */
  /* }; */
};
