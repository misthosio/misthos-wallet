type status =
  | None
  | LoadingIndex
  | LoadingVenture
  | CreatingVenture(string);

type state = {
  status,
  selected: option(Venture.t),
  index: Venture.Index.t,
  newVenture: string
};

type action =
  | IndexLoaded(Venture.Index.t)
  | VentureLoaded(Venture.t)
  | ChangeNewVenture(string)
  | SelectVenture(string)
  | AddVenture
  | VentureCreated(Venture.Index.t, Venture.t);

let component = ReasonReact.reducerComponent("Ventures");

let changeNewVenture = event =>
  ChangeNewVenture(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let selectVenture = e =>
  SelectVenture(ReactDOMRe.domElementToObj(ReactEventRe.Mouse.target(e))##id);

let make = (~session, _children) => {
  ...component,
  initialState: () => {
    newVenture: "",
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
                Venture.load(~ventureId=p.id)
                |> then_(venture => send(VentureLoaded(venture)) |> resolve)
                |> ignore
              )
            | _ => ()
            }
        )
      )
    | VentureLoaded(venture) =>
      ReasonReact.Update({...state, status: None, selected: Some(venture)})
    | VentureCreated(index, selected) =>
      ReasonReact.Update({
        ...state,
        status: None,
        index,
        selected: Some(selected)
      })
    | ChangeNewVenture(text) => ReasonReact.Update({...state, newVenture: text})
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
                Venture.load(~ventureId=id)
                |> then_(venture => send(VentureLoaded(venture)) |> resolve)
                |> ignore
              )
          )
        );
    | AddVenture =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {...state, status: CreatingVenture(name), newVenture: ""},
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
                ...state.index |> List.map(({name, id}) => (name, id))
              ]
            | _ => state.index |> List.map(({name, id}) => (name, id))
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
      <input
        placeholder="Create new Venture"
        value=state.newVenture
        onChange=(e => send(changeNewVenture(e)))
        autoFocus=Js.true_
      />
      <button onClick=(_e => send(AddVenture))>
        (ReasonReact.stringToElement("Add"))
      </button>
      venture
    </div>;
  }
};
