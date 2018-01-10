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

let applyToState = (issuerPubKey, event: Event.t, state) =>
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

let apply = (event, issuer, {state, log, watchers}) => {
  let log = log |> EventLog.append(event, issuer);
  let state = state |> applyToState(Utils.publicKeyFromKeyPair(issuer), event);
  watchers |> List.iter(w => w#receive(event));
  let watchers =
    switch (Watcher.initWatcherFor(event, log)) {
    | Some(w) => [w, ...watchers]
    | None => watchers
    };
  {log, state, watchers};
};

let reconstruct = log => {
  let {state} = make();
  let (state, watchers) =
    log
    |> EventLog.reduce(
         ((state, watchers), (issuer, event)) => (
           state |> applyToState(issuer, event),
           switch (Watcher.initWatcherFor(event, log)) {
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
    |> apply(ProjectCreated(projectCreated), session.appKeyPair)
    |> persist,
    Index.add(~projectId=projectCreated.projectId, ~projectName)
  ));
};

let suggestCandidate = (session: Session.data, candidateId, project) =>
  project
  |> apply(
       Event.makeCandidateSuggested(~candidateId, ~candidatePubKey=""),
       session.appKeyPair
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

let getId = ({state}) => state.id;

let getName = ({state}) => state.name;

let getMembers = ({state}) => state.members |> List.map(((_, m)) => m);

let getCandidates = ({state}) => state.candidates |> List.map(((_, c)) => c);
