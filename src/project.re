module Index = ProjectIndex;

type pubKey = string;

type member = {
  blockstackId: string,
  pubKey
};

type candidate = {
  blockstackId: string,
  pubKey,
  approvedBy: list(string)
};

type state = {
  id: string,
  name: string,
  members: list((pubKey, member)),
  candidates: list((string, candidate))
};

type t = {
  state,
  log: EventLog.t,
  watchers: list(Watcher.t)
};

let make = () => {
  state: {
    id: "",
    name: "",
    members: [],
    candidates: []
  },
  log: EventLog.make(),
  watchers: []
};

let memberIdFromPubKey = (pubKey, {members}) =>
  List.assoc(pubKey, members).blockstackId;

let applyToState = ({issuerPubKey, event}: EventLog.item, state) =>
  switch event {
  | ProjectCreated(created) => {
      ...state,
      id: created.projectId,
      name: created.projectName,
      members: [
        (
          created.creatorPubKey,
          {blockstackId: created.creatorId, pubKey: created.creatorPubKey}
        )
      ]
    }
  | CandidateSuggested(suggestion) => {
      ...state,
      candidates: [
        (
          suggestion.candidateId,
          {
            blockstackId: suggestion.candidateId,
            pubKey: suggestion.candidatePubKey,
            approvedBy: [memberIdFromPubKey(issuerPubKey, state)]
          }
        ),
        ...state.candidates
      ]
    }
  | CandidateApproved(approval) =>
    let candidates =
      state.candidates
      |> List.map(((id, c)) =>
           if (id == approval.candidateId) {
             (id, {...c, approvedBy: [approval.supporterId, ...c.approvedBy]});
           } else {
             (id, c);
           }
         );
    {...state, candidates};
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

let rec applyWatcherEvents = ({state, log, watchers} as project) => {
  let nextEvent =
    (
      try (
        Some(
          watchers
          |> List.rev
          |> List.find(w => w#resultingEvent() |> Js.Option.isSome)
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
    let state = state |> applyToState(item);
    let watchers = watchers |> updateWatchers(item, log);
    applyWatcherEvents({state, log, watchers});
  };
};

let apply = (issuer, event, {state, log, watchers}) => {
  let (item, log) = log |> EventLog.append(issuer, event);
  let state = state |> applyToState(item);
  let watchers = watchers |> updateWatchers(item, log);
  applyWatcherEvents({log, state, watchers});
};

let reconstruct = log => {
  let {state} = make();
  let (state, watchers) =
    log
    |> EventLog.reduce(
         ((state, watchers), item) => (
           state |> applyToState(item),
           switch (Watcher.initWatcherFor(item, log)) {
           | Some(w) => [w, ...watchers]
           | None => watchers
           }
         ),
         (state, [])
       );
  {log, state, watchers};
};

let persist = project =>
  Js.Promise.(
    Blockstack.putFile(
      project.state.id ++ "/log.json",
      EventLog.encode(project.log) |> Json.stringify,
      Js.false_
    )
    |> then_(() => resolve(project))
  );

let defaultPolicy = Policy.absolute;

let create = (session: Session.data, projectName) => {
  let projectCreated =
    Event.ProjectCreated.make(
      ~projectName,
      ~creatorId=session.userName,
      ~creatorPubKey=session.appKeyPair |> Utils.publicKeyFromKeyPair,
      ~metaPolicy=defaultPolicy
    );
  Js.Promise.all2((
    make()
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
         ~supporterId=session.userName,
         ~candidateId,
         ~candidatePubKey=""
       )
     )
  |> persist;

let load = id =>
  Js.Promise.(
    Blockstack.getFile(id ++ "/log.json", Js.false_)
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
let getId = ({state}) => state.id;

let getName = ({state}) => state.name;

let getMembers = ({state}) => state.members |> List.map(((_, m)) => m);

let getCandidates = ({state}) => state.candidates |> List.map(((_, c)) => c);
