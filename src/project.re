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
    let event = event =>
      switch event {
      | ProjectCreated(event) =>
        Json.Encode.(
          object_([
            ("type", string("ProjectCreated")),
            ("id", string(event.id)),
            ("name", string(event.name))
          ])
        )
      };
  };
  module Decode = {
    let event = raw => {
      let type_ = raw |> Json.Decode.(field("type", string));
      Json.Decode.(
        switch type_ {
        | "ProjectCreated" =>
          ProjectCreated({
            id: raw |> field("id", string),
            name: raw |> field("name", string)
          })
        }
      );
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

let applyToState = (event, _state) : state =>
  Event.(
    switch event {
    | ProjectCreated(created) => {id: created.id, name: created.name}
    }
  );

let apply = (event, issuer, {state, log}) => {
  log: log |> EventLog.append(event, issuer),
  state: state |> applyToState(event)
};

let reconstruct = log => {
  let {state} = make();
  {
    state:
      log
      |> EventLog.reduce((state, event) => state |> applyToState(event), state),
    log
  };
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

let createProject = (session, name) => {
  open Event;
  open Session;
  let appKeyPair = session.appKeyPair;
  let projectCreated = {id: Uuid.v4(), name};
  Js.Promise.all2((
    make() |> apply(ProjectCreated(projectCreated), appKeyPair) |> persist,
    Index.add(~id=projectCreated.id, ~name)
  ));
};

let load = id =>
  Js.Promise.(
    Blockstack.getFile(id ++ "/log.json", Js.false_)
    |> then_(nullLog =>
         switch (Js.Nullable.to_opt(nullLog)) {
         | Some(raw) =>
           resolve(raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct)
         }
       )
  );

let getState = project => project.state;
