module Index = ProjectIndex;

module EventLog = Log.Make(Event);

type pubKey = string;

type member = {
  blockstackId: string,
  pubKey,
  address: string,
  storageUrlPrefix: string
};

type candidate = {
  blockstackId: string,
  pubKey,
  address: string,
  storageUrlPrefix: string,
  approvedBy: list(string)
};

type state = {
  id: string,
  name: string,
  members: list((pubKey, member)),
  candidates: list(candidate)
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

let applyToState = (issuerPubKey, event, state) : state =>
  Event.(
    switch event {
    | ProjectCreated(created) => {
        ...state,
        id: created.projectId,
        name: created.projectName,
        members: [
          (
            created.creatorPubKey,
            {
              blockstackId: created.creatorId,
              pubKey: created.creatorPubKey,
              address: created.creatorPubKey |> Utils.addressFromPublicKey,
              storageUrlPrefix: created.creatorStorageUrlPrefix
            }
          )
        ]
      }
    | CandidateSuggested(suggestion) => {
        ...state,
        candidates: [
          {
            blockstackId: suggestion.candidateId,
            pubKey: suggestion.candidatePubKey,
            /* address: suggestion.candidatePubKey |> Utils.addressFromPublicKey, */
            address: "",
            storageUrlPrefix: suggestion.candidateStorageUrlPrefix,
            approvedBy: [memberIdFromPubKey(issuerPubKey, state)]
          },
          ...state.candidates
        ]
      }
    }
  );

let apply = (event, issuer, {state, log, watchers}) => {
  let log = log |> EventLog.append(event, issuer);
  let state = state |> applyToState(Utils.publicKeyFromKeyPair(issuer), event);
  watchers |> List.iter(w => w#receive(event));
  let watchers = Watcher.addWatcher(event, watchers);
  {log, state, watchers};
};

let reconstruct = log => {
  let {state} = make();
  let (state, watchers) =
    log
    |> EventLog.reduce(
         ((state, watchers), (issuer, event)) => {
           watchers |> List.iter(w => w#receive(event));
           (
             state |> applyToState(issuer, event),
             watchers |> Watcher.addWatcher(event)
           );
         },
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

let create = (session, projectName) => {
  open Session;
  let projectId = Uuid.v4();
  let projectCreated: Event.t =
    ProjectCreated({
      projectId,
      projectName,
      creatorId: session.userName,
      creatorPubKey: session.appKeyPair |> Utils.publicKeyFromKeyPair,
      creatorStorageUrlPrefix: "https://gaia.blockstack.org/hub/"
    });
  Js.Promise.all2((
    make() |> apply(projectCreated, session.appKeyPair) |> persist,
    Index.add(~projectId, ~projectName)
  ));
};

let suggestCandidate = (session: Session.data, blockstackId, project) =>
  project
  |> apply(
       CandidateSuggested({
         candidateId: blockstackId,
         candidatePubKey: "",
         candidateStorageUrlPrefix: ""
       }),
       session.appKeyPair
     )
  /* check policy and add member */
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

let getCandidates = ({state}) => state.candidates;
