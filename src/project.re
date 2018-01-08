module Index = {
  type item = {
    name: string,
    id: string
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
    Blockstack.putFile(
      indexPath,
      Encode.index(index) |> Json.stringify,
      Js.false_
    );
  let load = () =>
    Js.Promise.(
      Blockstack.getFile(indexPath, Js.false_)
      |> then_(nullProjects =>
           switch (Js.Nullable.to_opt(nullProjects)) {
           | None => persist([]) |> then_(() => resolve([]))
           | Some(index) => resolve(index |> Json.parseOrRaise |> Decode.index)
           }
         )
    );
};

type policy =
  | Policy;

type state = {policy};

module Event = {
  type t =
    | ProjectCreated(state);
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
      | "ProjectCreated" => ProjectCreated({policy: Policy})
      };
    };
  };
  let encode = Encode.event;
  let decode = Decode.event;
};

module EventLog = Log.Make(Event);

type t = {
  state,
  log: EventLog.t
};

let createProject = name => Index.load();
