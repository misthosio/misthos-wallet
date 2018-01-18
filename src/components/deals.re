type status =
  | None
  | LoadingIndex
  | LoadingDeal
  | CreatingDeal(string);

type state = {
  status,
  selected: option(Deal.t),
  index: Deal.Index.t,
  newDeal: string
};

type action =
  | IndexLoaded(Deal.Index.t)
  | DealLoaded(Deal.t)
  | ChangeNewDeal(string)
  | SelectDeal(string)
  | AddDeal
  | DealCreated(Deal.Index.t, Deal.t);

let component = ReasonReact.reducerComponent("Deals");

let changeNewDeal = event =>
  ChangeNewDeal(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
  );

let selectDeal = e =>
  SelectDeal(ReactDOMRe.domElementToObj(ReactEventRe.Mouse.target(e))##id);

let make = (~session, _children) => {
  ...component,
  initialState: () => {
    newDeal: "",
    status: LoadingIndex,
    index: [],
    selected: None
  },
  didMount: _self =>
    ReasonReact.SideEffects(
      ({send}) =>
        Js.Promise.(
          Deal.Index.load()
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
                Deal.load(~dealId=p.id)
                |> then_(deal => send(DealLoaded(deal)) |> resolve)
                |> ignore
              )
            | _ => ()
            }
        )
      )
    | DealLoaded(deal) =>
      ReasonReact.Update({...state, status: None, selected: Some(deal)})
    | DealCreated(index, selected) =>
      ReasonReact.Update({
        ...state,
        status: None,
        index,
        selected: Some(selected)
      })
    | ChangeNewDeal(text) => ReasonReact.Update({...state, newDeal: text})
    | SelectDeal(id) =>
      Js.log("SelectDeal(" ++ id ++ ")");
      let selectedId =
        switch state.selected {
        | Some(deal) => Deal.getId(deal)
        | None => ""
        };
      id == selectedId ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {...state, status: LoadingDeal, selected: None},
          (
            ({send}) =>
              Js.Promise.(
                Deal.load(~dealId=id)
                |> then_(deal => send(DealLoaded(deal)) |> resolve)
                |> ignore
              )
          )
        );
    | AddDeal =>
      switch (String.trim(state.newDeal)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        ReasonReact.UpdateWithSideEffects(
          {...state, status: CreatingDeal(name), newDeal: ""},
          (
            ({send}) =>
              Js.Promise.(
                Deal.Cmd.Create.exec(session, ~name)
                |> then_(((newIndex, deal)) =>
                     send(DealCreated(newIndex, deal)) |> resolve
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
      | (CreatingDeal(_), _) => "new"
      | (_, Some(deal)) => Deal.getId(deal)
      | _ => ""
      };
    let dealList =
      ReasonReact.arrayToElement(
        Array.of_list(
          Deal.Index.(
            switch state.status {
            | LoadingIndex => []
            | CreatingDeal(newDeal) => [
                (newDeal, "new"),
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
                 onClick=(e => send(selectDeal(e)))>
                 (ReasonReact.stringToElement(name))
               </li>
             )
        )
      );
    let status =
      switch state.status {
      | LoadingIndex => ReasonReact.stringToElement("Loading Index")
      | CreatingDeal(newDeal) =>
        ReasonReact.stringToElement("Creating deal '" ++ newDeal ++ "'")
      | _ => ReasonReact.stringToElement("deals:")
      };
    let deal =
      switch state.selected {
      | Some(deal) => <SelectedDeal deal session />
      | None => <div> (ReasonReact.stringToElement("Loading Deal")) </div>
      };
    <div>
      <h2> status </h2>
      <ul> dealList </ul>
      <input
        placeholder="Create new Deal"
        value=state.newDeal
        onChange=(e => send(changeNewDeal(e)))
        autoFocus=Js.true_
      />
      <button onClick=(_e => send(AddDeal))>
        (ReasonReact.stringToElement("Add"))
      </button>
      deal
    </div>;
  }
};
