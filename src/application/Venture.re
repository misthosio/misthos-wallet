module Index = Venture__Index;

module Validation = Venture__Validation;

type t = {
  id: string,
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
  let watchers = watchers |> updateWatchers(item, log);
  let state = state |> Validation.apply(event);
  let viewModel = viewModel |> ViewModel.apply(event);
  applyWatcherEvents({id, log, watchers, state, viewModel});
};

let reconstruct = log => {
  let {viewModel, state} = make("");
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
         ("", [], state, viewModel)
       );
  {id, log, watchers, state, viewModel};
};

let persist = ({id, log, state} as venture) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFile(id ++ "/log.json", logString)
      |> then_(() => resolve(venture))
    );
  state.partnerAddresses
  |> List.map(address =>
       Blockstack.putFile(id ++ "/" ++ address ++ "/log.json", logString)
     )
  |> ignore;
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (~ventureId) =>
  Js.Promise.(
    Blockstack.getFile(ventureId ++ "/log.json")
    |> then_(nullLog =>
         switch (Js.Nullable.to_opt(nullLog)) {
         | Some(raw) =>
           resolve(raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct)
         | None => raise(Not_found)
         }
       )
  );

let join = (session: Session.Data.t, ~blockstackId, ~ventureId) =>
  Js.Promise.(
    Blockstack.getFileFromUser(
      ventureId ++ "/" ++ session.address ++ "/log.json",
      ~username=blockstackId
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
         Index.add(~ventureId, ~ventureName=venture.state.ventureName)
         |> then_(index => resolve((index, venture)))
       )
  );

let getId = ({id}) => id;

let getViewModel = ({viewModel}) => viewModel;

module Synchronize = {
  let getPartnerHistoryUrls = (session: Session.Data.t, {id, state}) =>
    state.partnerIds
    |> List.filter(partnerId => partnerId != session.blockstackId)
    |> List.map(partnerId =>
         Blockstack.getUserAppFileUrl(
           ~path=id ++ "/" ++ session.address ++ "/log.json",
           ~username=partnerId,
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
               /* When the issuerPubKey is not recognized ignore the event */
               | InvalidIssuer => (venture, None)
               | PartnerApprovalPolicyConflict(_, _) as conflict => (
                   venture,
                   Some(Error(venture, item, conflict))
                 )
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
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.blockstackId,
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
  module SuggestProspect = {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec = (session: Session.Data.t, ~prospectId, {state} as venture) =>
      Js.Promise.(
        UserPublicInfo.read(~blockstackId=prospectId)
        |> then_(readResult =>
             switch readResult {
             | UserPublicInfo.Ok(info) =>
               venture
               |> apply(
                    session.appKeyPair,
                    Event.makeProspectSuggested(
                      ~supporterId=session.blockstackId,
                      ~prospectId,
                      ~prospectPubKey=info.appPubKey,
                      ~policy=state.metaPolicy
                    )
                  )
               |> persist
               |> then_(p => resolve(Ok(p)))
             | UserPublicInfo.NotFound => resolve(NoUserInfo)
             }
           )
      );
  };
  module ApproveProspect = {
    type result =
      | Ok(t);
    let exec = (session: Session.Data.t, ~prospectId, {state} as venture) =>
      Js.Promise.(
        venture
        |> apply(
             session.appKeyPair,
             Event.makeProspectApproved(
               ~processId=Validation.processIdForProspect(prospectId, state),
               ~prospectId,
               ~supporterId=session.blockstackId
             )
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
  };
  module Synchronize = Synchronize;
};
