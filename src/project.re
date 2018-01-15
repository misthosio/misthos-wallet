module Index = ProjectIndex;

type t = {
  id: string,
  log: EventLog.t,
  watchers: list(Watcher.t),
  viewModel: ViewModel.t
};

let make = id => {
  id,
  log: EventLog.make(),
  watchers: [],
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

let rec applyWatcherEvents = ({log, watchers, viewModel} as project) => {
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
  | None => project
  | Some((issuer, event)) =>
    let (item, log) = log |> EventLog.append(issuer, event);
    let watchers = watchers |> updateWatchers(item, log);
    let viewModel = viewModel |> ViewModel.apply(event);
    applyWatcherEvents({...project, viewModel, log, watchers});
  };
};

let apply = (issuer, event, {log, watchers, viewModel} as project) => {
  let (item, log) = log |> EventLog.append(issuer, event);
  let watchers = watchers |> updateWatchers(item, log);
  let viewModel = viewModel |> ViewModel.apply(event);
  applyWatcherEvents({...project, log, watchers, viewModel});
};

let reconstruct = log => {
  let {viewModel} = make("");
  let (id, watchers, viewModel) =
    log
    |> EventLog.reduce(
         ((id, watchers, viewModel), item) => (
           switch item.event {
           | ProjectCreated({projectId}) => projectId
           | _ => id
           },
           switch (Watcher.initWatcherFor(item, log)) {
           | Some(w) => [w, ...watchers]
           | None => watchers
           },
           viewModel |> ViewModel.apply(item.event)
         ),
         ("", [], viewModel)
       );
  {id, log, watchers, viewModel};
};

let persist = project =>
  Js.Promise.(
    Blockstack.putFile(
      project.id ++ "/log.json",
      EventLog.encode(project.log) |> Json.stringify
    )
    |> then_(() => resolve(project))
  );

let defaultPolicy = Policy.absolute;

let load = id =>
  Js.Promise.(
    Blockstack.getFile(id ++ "/log.json")
    |> then_(nullLog =>
         switch (Js.Nullable.to_opt(nullLog)) {
         | Some(raw) =>
           resolve(raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct)
         | None => raise(Not_found)
         }
       )
  );

/* let sync = (otherLogs, {state, watchers, log} as project) => { */
/*   let (state,log) = log |> EventLog.merge(otherLogs,,item) => Verification.includeItem(item),((state,log),item) => { */
/*            let state = state |> applyToState(item); */
/*            let watchers = watchers |> updateWatchers(item, log); */
/*            state */
/*     } */
/* let newItems = log |> EventLog.findNewItems(otherLogs); */
/* let project = */
/*   newItems */
/*   |> List.fold_left( */
/*        ({state, watchers, log}, item) => { */
/*          /1* Verification.includeItem(item); *1/ */
/*          let log = log |> EventLog.appendItem(item); */
/*          let state = state |> applyToState(item); */
/*          let watchers = watchers |> updateWatchers(item, log); */
/*          {state, watchers, log}; */
/*        }, */
/*        project */
/*      ); */
/* applyWatcherEvents({log, state, watchers}); */
/* }; */
let getId = ({id}) => id;

let getViewModel = ({viewModel}) => viewModel;

module Command = {
  let create = (session: Session.data, projectName) => {
    let projectCreated =
      Event.ProjectCreated.make(
        ~projectName,
        ~creatorId=session.blockstackId,
        ~creatorPubKey=session.appKeyPair |> Utils.publicKeyFromKeyPair,
        ~metaPolicy=defaultPolicy
      );
    Js.Promise.all2((
      make(projectCreated.projectId)
      |> apply(session.appKeyPair, ProjectCreated(projectCreated))
      |> persist,
      Index.add(~projectId=projectCreated.projectId, ~projectName)
    ));
  };
  let suggestCandidate = (session: Session.data, candidateId, project) =>
    project
    |> apply(
         session.appKeyPair,
         Event.makeCandidateSuggested(
           ~supporterId=session.blockstackId,
           ~candidateId,
           ~candidatePubKey=""
         )
       )
    |> persist;
};
