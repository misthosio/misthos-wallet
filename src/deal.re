module Index = DealIndex;

type t = {
  id: string,
  log: EventLog.t,
  watchers: list(Watcher.t),
  state: ValidationState.t,
  viewModel: ViewModel.t
};

let make = id => {
  id,
  log: EventLog.make(),
  watchers: [],
  state: ValidationState.make(),
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
    let state = state |> ValidationState.apply(event);
    let viewModel = viewModel |> ViewModel.apply(event);
    applyWatcherEvents({id, log, watchers, state, viewModel});
  };
};

let apply = (issuer, event, {id, log, watchers, state, viewModel}) => {
  let (item, log) = log |> EventLog.append(issuer, event);
  let watchers = watchers |> updateWatchers(item, log);
  let state = state |> ValidationState.apply(event);
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
           | DealCreated({dealId}) => dealId
           | _ => id
           },
           switch (Watcher.initWatcherFor(item, log)) {
           | Some(w) => [w, ...watchers]
           | None => watchers
           },
           state |> ValidationState.apply(event),
           viewModel |> ViewModel.apply(event)
         ),
         ("", [], state, viewModel)
       );
  {id, log, watchers, state, viewModel};
};

let persist = ({id, log, state} as deal) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFile(id ++ "/log.json", logString)
      |> then_(() => resolve(deal))
    );
  state.partners
  |> List.map(({address}: ValidationState.partner) =>
       Blockstack.putFile(id ++ "/" ++ address ++ "/log.json", logString)
     )
  |> ignore;
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (~dealId) =>
  Js.Promise.(
    Blockstack.getFile(dealId ++ "/log.json")
    |> then_(nullLog =>
         switch (Js.Nullable.to_opt(nullLog)) {
         | Some(raw) =>
           resolve(raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct)
         | None => raise(Not_found)
         }
       )
  );

let getId = ({id}) => id;

let getViewModel = ({viewModel}) => viewModel;

module Synchronize = {
  let getPartnerHistoryUrls = (session: Session.Data.t, {id, state}) =>
    state.partners
    |> List.filter(({blockstackId}: ValidationState.partner) =>
         blockstackId != session.blockstackId
       )
    |> List.map(({blockstackId}: ValidationState.partner) =>
         Blockstack.getUserAppFileUrl(
           ~path=id ++ "/" ++ session.address ++ "/log.json",
           ~username=blockstackId,
           ~appOrigin=Utils.origin
         )
       )
    |> Array.of_list
    |> Js.Promise.all;
  type result =
    | Ok(t);
  let exec = (otherLogs, {log} as deal) => {
    let newItems = log |> EventLog.findNewItems(otherLogs);
    let deal =
      newItems
      |> List.fold_left(
           (
             {log, watchers, state, viewModel} as deal,
             {event} as item: EventLog.item
           ) =>
             switch (item |> ValidationState.validate(state)) {
             | Ok =>
               let log = log |> EventLog.appendItem(item);
               let watchers = watchers |> updateWatchers(item, log);
               let state = state |> ValidationState.apply(event);
               let viewModel = viewModel |> ViewModel.apply(event);
               {...deal, log, watchers, state, viewModel};
             },
           deal
         );
    Js.Promise.(
      applyWatcherEvents(deal) |> persist |> then_(p => Ok(p) |> resolve)
    );
  };
};

let getPartnerHistoryUrls = Synchronize.getPartnerHistoryUrls;

module Cmd = {
  module Create = {
    type result = (Index.t, t);
    let exec = (session: Session.Data.t, ~name as dealName) => {
      let dealCreated =
        Event.DealCreated.make(
          ~dealName,
          ~creatorId=session.blockstackId,
          ~creatorPubKey=session.appKeyPair |> Utils.publicKeyFromKeyPair,
          ~metaPolicy=defaultPolicy
        );
      Js.Promise.all2((
        Index.add(~dealId=dealCreated.dealId, ~dealName),
        make(dealCreated.dealId)
        |> apply(session.appKeyPair, DealCreated(dealCreated))
        |> persist
      ));
    };
  };
  module SuggestProspect = {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec = (session: Session.Data.t, ~prospectId, deal) =>
      Js.Promise.(
        UserPublicInfo.read(~blockstackId=prospectId)
        |> then_(readResult =>
             switch readResult {
             | UserPublicInfo.Ok(info) =>
               deal
               |> apply(
                    session.appKeyPair,
                    Event.makeProspectSuggested(
                      ~supporterId=session.blockstackId,
                      ~prospectId,
                      ~prospectPubKey=info.appPubKey
                    )
                  )
               |> persist
               |> then_(p => resolve(Ok(p)))
             | UserPublicInfo.NotFound => resolve(NoUserInfo)
             }
           )
      );
  };
  module Synchronize = Synchronize;
};
