module Index = {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  module Encode = {
    let item = item =>
      Json.Encode.(
        object_([("name", string(item.name)), ("id", string(item.id))])
      );
    let index = Json.Encode.list(item);
  };
  module Decode = {
    let item = json =>
      Json.Decode.{
        name: json |> field("name", string),
        id: json |> field("id", string)
      };
    let index = Json.Decode.list(item);
  };
  let indexPath = "index.json";
  let persist = index =>
    Js.Promise.(
      Blockstack.putFile(
        indexPath,
        Encode.index(index) |> Json.stringify,
        Js.false_
      )
      |> then_(() => resolve(index))
    );
  let load = () =>
    Js.Promise.(
      Blockstack.getFile(indexPath, Js.false_)
      |> then_(nullProjects =>
           switch (Js.Nullable.to_opt(nullProjects)) {
           | None => persist([])
           | Some(index) => resolve(index |> Json.parseOrRaise |> Decode.index)
           }
         )
    );
  let add = (~id, ~name) =>
    Js.Promise.(load() |> then_(index => [{id, name}, ...index] |> persist));
};

module Event = {
  type projectCreated = {
    projectId: string,
    projectName: string,
    creatorId: string,
    creatorPubKey: string,
    creatorStorageUrlPrefix: string
  };
  type candidateSuggested = {
    candidateId: string,
    candidatePubKey: string,
    candidateStorageUrlPrefix: string
  };
  type t =
    | ProjectCreated(projectCreated)
    | CandidateSuggested(candidateSuggested);
  module Encode = {
    let projectCreated = event =>
      Json.Encode.(
        object_([
          ("type", string("ProjectCreated")),
          ("projectId", string(event.projectId)),
          ("projectName", string(event.projectName)),
          ("creatorId", string(event.creatorId)),
          ("creatorPubKey", string(event.creatorPubKey)),
          ("creatorStorageUrlPrefix", string(event.creatorStorageUrlPrefix))
        ])
      );
    let candidateSuggested = event =>
      Json.Encode.(
        object_([
          ("type", string("CandidateSuggested")),
          ("candidateId", string(event.candidateId)),
          ("candidatePubKey", string(event.candidatePubKey)),
          (
            "candidateStorageUrlPrefix",
            string(event.candidateStorageUrlPrefix)
          )
        ])
      );
    let event = event =>
      switch event {
      | ProjectCreated(event) => projectCreated(event)
      | CandidateSuggested(event) => candidateSuggested(event)
      };
  };
  module Decode = {
    let projectCreated = raw =>
      Json.Decode.{
        projectId: raw |> field("projectId", string),
        projectName: raw |> field("projectName", string),
        creatorId: raw |> field("creatorId", string),
        creatorPubKey: raw |> field("creatorPubKey", string),
        creatorStorageUrlPrefix:
          raw |> field("creatorStorageUrlPrefix", string)
      };
    let candidateSuggested = raw =>
      Json.Decode.{
        candidateId: raw |> field("candidateId", string),
        candidatePubKey: raw |> field("candidatePubKey", string),
        candidateStorageUrlPrefix:
          raw |> field("candidateStorageUrlPrefix", string)
      };
    let event = raw => {
      let type_ = raw |> Json.Decode.(field("type", string));
      switch type_ {
      | "ProjectCreated" => ProjectCreated(projectCreated(raw))
      | "CandidateSuggested" => CandidateSuggested(candidateSuggested(raw))
      };
    };
  };
  let encode = Encode.event;
  let decode = Decode.event;
};

module EventLog = Log.Make(Event);

module Watcher = {
  type t = {. receive: Event.t => unit};
  module CandidateApproval = {
    let make = suggestion => {
      val suggestion = suggestion;
      val approval = ref(1);
      pub receive = event => approval := approval^ + 1
    };
  };
  let addWatcher = (event, watchers) =>
    Event.(
      switch event {
      | CandidateSuggested(suggestion) => [
          CandidateApproval.make(suggestion),
          ...watchers
        ]
      | _ => watchers
      }
    );
};

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
  open Event;
  let projectCreated = {
    projectId: Uuid.v4(),
    projectName,
    creatorId: session.userName,
    creatorPubKey: session.appKeyPair |> Utils.publicKeyFromKeyPair,
    creatorStorageUrlPrefix: "https://gaia.blockstack.org/hub/"
  };
  Js.Promise.all2((
    make()
    |> apply(ProjectCreated(projectCreated), session.appKeyPair)
    |> persist,
    Index.add(~id=projectCreated.projectId, ~name=projectName)
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
