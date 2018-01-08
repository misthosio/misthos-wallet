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
    id: string,
    name: string
  };
  type t =
    | ProjectCreated(projectCreated);
  module Encode = {
    let type_ = event =>
      Json.Encode.string(
        switch event {
        | ProjectCreated(_) => "ProjectCreated"
        }
      );
    let event = event => Json.Encode.(object_([("type", type_(event))]));
  };
  module Decode = {
    let event = raw => {
      let type_ = raw |> Json.Decode.(field("type", string));
      switch type_ {
      | "ProjectCreated" => ProjectCreated({id: "", name: ""})
      };
    };
  };
  let encode = Encode.event;
  let decode = Decode.event;
};

module EventLog = Log.Make(Event);

type state = {
  id: string,
  name: string
};

type t = {
  state,
  log: EventLog.t
};

let make = () => {
  state: {
    id: "",
    name: ""
  },
  log: EventLog.make()
};

let apply = (event, issuer, {state, log}) =>
  Event.(
    switch event {
    | ProjectCreated(created) => {
        log: log |> EventLog.append(event, issuer),
        state: {
          id: created.id,
          name: created.name
        }
      }
    }
  );

let persist = project =>
  Js.Promise.(
    Blockstack.putFile(
      project.state.id ++ "/log.json",
      EventLog.encode(project.log) |> Json.stringify,
      Js.false_
    )
    |> then_(() => resolve(project))
  );

let createProject = (session, name) => {
  open Event;
  open Session;
  let appKeyPair = session.appKeyPair;
  let projectCreated = {id: Uuid.v4(), name};
  let persisting =
    make() |> apply(ProjectCreated(projectCreated), appKeyPair) |> persist;
  let added = Index.add(~id=projectCreated.id, ~name);
  Js.Promise.all2((persisting, added));
};
