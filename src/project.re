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

type member = {
  blockstackId: string,
  appPublicKey: string,
  address: string,
  storageUrlPrefix: string
};

module Event = {
  type projectCreated = {
    id: string,
    name: string,
    creator: member
  };
  type t =
    | ProjectCreated(projectCreated);
  module Encode = {
    let member = member =>
      Json.Encode.(
        object_([
          ("blockstackId", string(member.blockstackId)),
          ("appPublicKey", string(member.appPublicKey)),
          ("address", string(member.address)),
          ("storageUrlPrefix", string(member.storageUrlPrefix))
        ])
      );
    let event = event =>
      switch event {
      | ProjectCreated(event) =>
        Json.Encode.(
          object_([
            ("type", string("ProjectCreated")),
            ("id", string(event.id)),
            ("name", string(event.name)),
            ("creator", member(event.creator))
          ])
        )
      };
  };
  module Decode = {
    let member = raw =>
      Json.Decode.{
        blockstackId: raw |> field("blockstackId", string),
        appPublicKey: raw |> field("appPublicKey", string),
        address: raw |> field("address", string),
        storageUrlPrefix: raw |> field("storageUrlPrefix", string)
      };
    let event = raw => {
      let type_ = raw |> Json.Decode.(field("type", string));
      Json.Decode.(
        switch type_ {
        | "ProjectCreated" =>
          ProjectCreated({
            id: raw |> field("id", string),
            name: raw |> field("name", string),
            creator: raw |> field("creator", member)
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
  name: string,
  members: list(member)
};

type t = {
  state,
  log: EventLog.t
};

let make = () => {
  state: {
    id: "",
    name: "",
    members: []
  },
  log: EventLog.make()
};

let applyToState = (event, _state) : state =>
  Event.(
    switch event {
    | ProjectCreated(created) => {
        id: created.id,
        name: created.name,
        members: [created.creator]
      }
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

let create = (session, name) =>
  Event.(
    Session.(
      Blockstack.getOrSetLocalGaiaHubConnection()
      |> Js.Promise.then_(gaiaCon => {
           let projectCreated = {
             id: Uuid.v4(),
             name,
             creator: {
               blockstackId: session.userName,
               appPublicKey: session.appKeyPair |> Utils.publicKeyFromKeyPair,
               address: gaiaCon##address,
               storageUrlPrefix: gaiaCon##url_prefix
             }
           };
           Js.Promise.all2((
             make()
             |> apply(ProjectCreated(projectCreated), session.appKeyPair)
             |> persist,
             Index.add(~id=projectCreated.id, ~name)
           ));
         })
    )
  );

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

let getState = project => project.state;
