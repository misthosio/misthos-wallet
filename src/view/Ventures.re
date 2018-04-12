open PrimitiveTypes;

type status =
  | None
  | LoadingIndex
  | LoadingVenture
  | JoiningVenture
  | CreatingVenture(string);

type state = {
  status,
  selected: option(Venture.t),
  index: Venture.Index.t,
  newVenture: string,
  joinVentureId: string,
  joinVentureUserId: string
};

type action =
  | IndexLoaded(Venture.Index.t)
  | VentureLoaded(Venture.t)
  | ChangeNewVenture(string)
  | ChangeJoinVentureUserId(string)
  | ChangeJoinVentureId(string)
  | SelectVenture(string)
  | JoinVenture
  | CreateVenture
  | VentureCreated(Venture.Index.t, Venture.t)
  | VentureJoined(Venture.Index.t, Venture.t);

let component = ReasonReact.reducerComponent("Ventures");

let formText = event => ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event)
                        )##value;

let selectVenture = e =>
  SelectVenture(ReactDOMRe.domElementToObj(ReactEventRe.Mouse.target(e))##id);

let make = (~session, _children) => {
  ...component,
  initialState: () => {
    newVenture: "",
    joinVentureId: "",
    joinVentureUserId: "",
    status: LoadingIndex,
    index: [],
    selected: None
  },
  didMount: _self =>
    ReasonReact.SideEffects(
      ({send}) =>
        Js.Promise.(
          Venture.Index.load()
          |> then_(index => send(IndexLoaded(index)) |> resolve)
          |> ignore
        )
    ),
  reducer: (action, state) =>
    switch action {
    | IndexLoaded(index) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: None, index},
        (
          ({send}) =>
            switch index {
            | [p, ..._rest] =>
              Js.Promise.(
                Venture.load(session, ~ventureId=p.id)
                |> then_(venture => send(VentureLoaded(venture)) |> resolve)
                |> ignore
              )
            | _ => ()
            }
        )
      )
    | VentureLoaded(venture) =>
      ReasonReact.Update({...state, status: None, selected: Some(venture)})
    | VentureJoined(index, selected)
    | VentureCreated(index, selected) =>
      ReasonReact.Update({
        ...state,
        status: None,
        index,
        selected: Some(selected)
      })
    | ChangeNewVenture(text) =>
      ReasonReact.Update({...state, newVenture: text})
    | ChangeJoinVentureUserId(text) =>
      ReasonReact.Update({...state, joinVentureUserId: text})
    | ChangeJoinVentureId(text) =>
      ReasonReact.Update({...state, joinVentureId: text})
    | SelectVenture(id) =>
      Js.log("SelectVenture(" ++ id ++ ")");
      let selectedId =
        switch state.selected {
        | Some(venture) => Venture.getId(venture)
        | None => ""
        };
      id == selectedId ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {...state, status: LoadingVenture, selected: None},
          (
            ({send}) =>
              Js.Promise.(
                Venture.load(session, ~ventureId=id |> VentureId.fromString)
                |> then_(venture => send(VentureLoaded(venture)) |> resolve)
                |> ignore
              )
          )
        );
    | CreateVenture =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            status: CreatingVenture(name),
            selected: None,
            newVenture: ""
          },
          (
            ({send}) =>
              Js.Promise.(
                Venture.Cmd.Create.exec(session, ~name)
                |> then_(((newIndex, venture)) =>
                     send(VentureCreated(newIndex, venture)) |> resolve
                   )
                |> ignore
              )
          )
        )
      }
    | JoinVenture =>
      switch (
        String.trim(state.joinVentureUserId),
        String.trim(state.joinVentureId)
      ) {
      | ("", _) => ReasonReact.NoUpdate
      | (_, "") => ReasonReact.NoUpdate
      | (userId, ventureId) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            status: JoiningVenture,
            selected: None,
            joinVentureUserId: "",
            joinVentureId: ""
          },
          (
            ({send}) =>
              Js.Promise.(
                Venture.join(session, ~userId, ~ventureId)
                |> then_(((newIndex, venture)) =>
                     send(VentureJoined(newIndex, venture)) |> resolve
                   )
                |> ignore
              )
          )
        )
      }
    },
  render: ({send, state}) => {
    let selectedId =
      switch (state.status, state.selected) {
      | (CreatingVenture(_), _) => "new"
      | (_, Some(venture)) => Venture.getId(venture)
      | _ => ""
      };
    let ventureList =
      ReasonReact.arrayToElement(
        Array.of_list(
          Venture.Index.(
            switch state.status {
            | LoadingIndex => []
            | CreatingVenture(newVenture) => [
                (newVenture, "new"),
                ...state.index
                   |> List.map(({name, id}) =>
                        (name, id |> VentureId.toString)
                      )
              ]
            | _ =>
              state.index
              |> List.map(({name, id}) => (name, id |> VentureId.toString))
            }
          )
          |> List.map(((name, id)) =>
               <li
                 key=id
                 id
                 className=(id == selectedId ? "selected" : "")
                 onClick=(e => send(selectVenture(e)))>
                 (ReasonReact.stringToElement(name))
               </li>
             )
        )
      );
    let status =
      switch state.status {
      | LoadingIndex => ReasonReact.stringToElement("Loading Index")
      | CreatingVenture(newVenture) =>
        ReasonReact.stringToElement("Creating venture '" ++ newVenture ++ "'")
      | _ => ReasonReact.stringToElement("ventures:")
      };
    let venture =
      switch state.selected {
      | Some(venture) => <SelectedVenture venture session />
      | None => <div> (ReasonReact.stringToElement("Loading Venture")) </div>
      };
    <div>
      <h2> status </h2>
      <ul> ventureList </ul>
      <div>
        <input
          placeholder="Create new Venture"
          value=state.newVenture
          onChange=(e => send(ChangeNewVenture(formText(e))))
          autoFocus=Js.true_
        />
        <button onClick=(_e => send(CreateVenture))>
          (ReasonReact.stringToElement("create"))
        </button>
      </div>
      <div>
        <input
          placeholder="Join Venture User"
          value=state.joinVentureUserId
          onChange=(e => send(ChangeJoinVentureUserId(formText(e))))
          autoFocus=Js.true_
        />
        <input
          placeholder="Join Venture Id"
          value=state.joinVentureId
          onChange=(e => send(ChangeJoinVentureId(formText(e))))
          autoFocus=Js.true_
        />
        <button onClick=(_e => send(JoinVenture))>
          (ReasonReact.stringToElement("join"))
        </button>
      </div>
      venture
    </div>;
  }
};
