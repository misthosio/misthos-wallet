open PrimitiveTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

exception InvalidEvent(Validation.result);

type t = {
  id: ventureId,
  log: EventLog.t,
  watchers: list(Watcher.t),
  state: Validation.state,
  viewModel: ViewModel.t
};

let make = id => {
  id,
  log: EventLog.make(),
  watchers: [],
  state: Validation.makeState(),
  viewModel: ViewModel.make()
};

let updateWatchers = (item, log, watchers) => {
  watchers |> List.iter(w => w#receive(item));
  (
    switch (Watcher.initWatcherFor(item, log)) {
    | Some(w) => [w, ...watchers]
    | None => watchers
    }
  )
  |> List.filter(w => w#processCompleted() == false);
};

let rec applyWatcherEvents = ({id, log, watchers, state, viewModel}) => {
  let nextEvent =
    (
      try (
        Some(
          watchers
          |> List.rev
          |> List.find(w => w#pendingEvent() |> Js.Option.isSome)
        )
      ) {
      | Not_found => None
      }
    )
    |> DoNotFormat.andThenGetEvent;
  switch nextEvent {
  | None => {id, log, watchers, state, viewModel}
  | Some((issuer, event)) =>
    let (item, log) = log |> EventLog.append(issuer, event);
    let watchers = watchers |> updateWatchers(item, log);
    let state = state |> Validation.apply(event);
    let viewModel = viewModel |> ViewModel.apply(event);
    applyWatcherEvents({id, log, watchers, state, viewModel});
  };
};

let apply = (issuer, event, {id, log, watchers, state, viewModel}) => {
  let (item, log) = log |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(state)) {
  | Ok =>
    let watchers = watchers |> updateWatchers(item, log);
    let state = state |> Validation.apply(event);
    let viewModel = viewModel |> ViewModel.apply(event);
    applyWatcherEvents({id, log, watchers, state, viewModel});
  /* This should never happen / only incase of an UI input bug!!! */
  | result => raise(InvalidEvent(result))
  };
};

let reconstruct = log => {
  let {viewModel, state} = make(VentureId.make());
  let (id, watchers, state, viewModel) =
    log
    |> EventLog.reduce(
         ((id, watchers, state, viewModel), {event} as item) => (
           switch event {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           switch (Watcher.initWatcherFor(item, log)) {
           | Some(w) => [w, ...watchers]
           | None => watchers
           },
           state |> Validation.apply(event),
           viewModel |> ViewModel.apply(event)
         ),
         (VentureId.make(), [], state, viewModel)
       );
  {id, log, watchers, state, viewModel};
};

let persist = ({id, log, state} as venture) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let summaryString =
    log |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFile((id |> VentureId.toString) ++ "/log.json", logString)
      |> then_(() => resolve(venture))
    );
  Js.Promise.(
    state.partnerAddresses
    |> List.fold_left(
         (promise, address) =>
           promise
           |> then_(() =>
                Blockstack.putFile(
                  (id |> VentureId.toString) ++ "/" ++ address ++ "/log.json",
                  logString
                )
              )
           |> then_(() =>
                Blockstack.putFile(
                  (id |> VentureId.toString)
                  ++ "/"
                  ++ address
                  ++ "/summary.json",
                  summaryString
                )
              ),
         resolve()
       )
    |> ignore
  );
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (~ventureId) =>
  Js.Promise.(
    Blockstack.getFile((ventureId |> VentureId.toString) ++ "/log.json")
    |> then_(nullLog =>
         switch (Js.Nullable.to_opt(nullLog)) {
         | Some(raw) =>
           resolve(raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct)
         | None => raise(Not_found)
         }
       )
  );

let join = (session: Session.Data.t, ~userId, ~ventureId) =>
  Js.Promise.(
    Blockstack.getFileFromUser(
      ventureId ++ "/" ++ session.address ++ "/log.json",
      ~username=userId
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.to_opt(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) =>
           raw
           |> Json.parseOrRaise
           |> EventLog.decode
           |> reconstruct
           |> persist
         }
       )
    |> then_(venture =>
         Index.add(
           ~ventureId=venture.id,
           ~ventureName=venture.state.ventureName
         )
         |> then_(index => resolve((index, venture)))
       )
  );

let getId = ({id}) => id |> VentureId.toString;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getViewModel = ({viewModel}) => viewModel;

module Synchronize = {
  let getPartnerHistoryUrls = (session: Session.Data.t, {id, state}) =>
    state.partnerIds
    |> List.filter(partnerId => UserId.neq(partnerId, session.userId))
    |> List.map(partnerId =>
         Blockstack.getUserAppFileUrl(
           ~path=(id |> VentureId.toString) ++ "/" ++ session.address,
           ~username=partnerId |> UserId.toString,
           ~appOrigin=Location.origin
         )
       )
    |> Array.of_list
    |> Js.Promise.all;
  type result =
    | Ok(t)
    | Error(t, EventLog.item, Validation.result);
  let exec = (otherLogs, {log} as venture) => {
    let newItems = log |> EventLog.findNewItems(otherLogs);
    let (venture, error) =
      newItems
      |> List.fold_left(
           (
             ({log, watchers, state, viewModel} as venture, error),
             {event} as item: EventLog.item
           ) =>
             if (Js.Option.isSome(error)) {
               (venture, error);
             } else {
               switch (item |> Validation.validate(state)) {
               | Ok =>
                 let log = log |> EventLog.appendItem(item);
                 let watchers = watchers |> updateWatchers(item, log);
                 let state = state |> Validation.apply(event);
                 let viewModel = viewModel |> ViewModel.apply(event);
                 ({...venture, log, watchers, state, viewModel}, None);
               | PolicyMissmatch as conflict => (
                   venture,
                   Some(Error(venture, item, conflict))
                 )
               | PolicyNotFulfilled as conflict => (
                   venture,
                   Some(Error(venture, item, conflict))
                 )
               /* Ignored validation issues */
               | InvalidIssuer =>
                 logMessage("Invalid issuer detected");
                 (venture, None);
               | BadData =>
                 logMessage("Bad data in event detected");
                 (venture, None);
               | UnknownProcessId =>
                 logMessage("Unknown ProcessId detected");
                 (venture, None);
               | DuplicateApproval =>
                 logMessage("Duplicate Approval detected");
                 (venture, None);
               };
             },
           (venture, None)
         );
    Js.Promise.(
      applyWatcherEvents(venture)
      |> persist
      |> then_(p =>
           (
             switch error {
             | None => Ok(p)
             | Some(e) => e
             }
           )
           |> resolve
         )
    );
  };
};

let getPartnerHistoryUrls = Synchronize.getPartnerHistoryUrls;

module Cmd = {
  module Create = {
    type result = (Index.t, t);
    let exec = (session: Session.Data.t, ~name as ventureName) => {
      logMessage("Executing 'Create' command");
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.userId,
          ~creatorPubKey=session.appKeyPair |> Utils.publicKeyFromKeyPair,
          ~metaPolicy=defaultPolicy
        );
      Js.Promise.all2((
        Index.add(~ventureId=ventureCreated.ventureId, ~ventureName),
        make(ventureCreated.ventureId)
        |> apply(session.appKeyPair, VentureCreated(ventureCreated))
        |> persist
      ));
    };
  };
  module Synchronize = Synchronize;
  module SuggestProspect = {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec = (session: Session.Data.t, ~prospectId, {state} as venture) => {
      logMessage("Executing 'SuggestProspect' command");
      Js.Promise.(
        UserPublicInfo.read(~blockstackId=prospectId)
        |> then_(readResult =>
             switch readResult {
             | UserPublicInfo.Ok(info) =>
               venture
               |> apply(
                    session.appKeyPair,
                    Event.makeProspectSuggested(
                      ~supporterId=session.userId,
                      ~prospectId,
                      ~prospectPubKey=info.appPubKey,
                      ~policy=state.addPartnerPolicy
                    )
                  )
               |> persist
               |> then_(p => resolve(Ok(p)))
             | UserPublicInfo.NotFound => resolve(NoUserInfo)
             }
           )
      );
    };
  };
  module ApproveProspect = {
    type result =
      | Ok(t);
    let exec = (session: Session.Data.t, ~processId, venture) => {
      logMessage("Executing 'ApproveProspect' command");
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makeProspectApproved(
               ~processId,
               ~supporterId=session.userId
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module SuggestPartnerLabel = {
    type result =
      | Ok(t);
    let exec =
        (session: Session.Data.t, ~partnerId, ~labelId, {state} as venture) => {
      logMessage("Executing 'SuggestPartnerLabel' command");
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makePartnerLabelSuggested(
               ~partnerId,
               ~supporterId=session.userId,
               ~labelId,
               ~policy=state.addPartnerLabelPolicy
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module ApprovePartnerLabel = {
    type result =
      | Ok(t);
    let exec = (session: Session.Data.t, ~processId, venture) => {
      logMessage("Executing 'ApprovePartnerLabel' command");
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makePartnerLabelApproved(
               ~processId,
               ~supporterId=session.userId
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module SubmitContribution = {
    type result =
      | Ok(t);
    let exec =
        (
          session: Session.Data.t,
          ~amountInteger: int,
          ~amountFraction: int,
          ~currency: string,
          ~description: string,
          {state} as venture
        ) => {
      logMessage("Executing 'SubmitContribution' command");
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makeContributionSubmitted(
               ~submitterId=session.userId,
               ~amountInteger,
               ~amountFraction,
               ~currency,
               ~description,
               ~policy=state.acceptContributionPolicy
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module ApproveContribution = {
    type result =
      | Ok(t);
    let exec = (session: Session.Data.t, ~processId, venture) => {
      logMessage("Executing 'ApproveContribution' command");
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makeContributionApproved(
               ~processId,
               ~supporterId=session.userId
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
};
