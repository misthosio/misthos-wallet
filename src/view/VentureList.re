open PrimitiveTypes;

type status =
  | None
  | LoadingIndex;

type state = {
  status,
  selected: option(ventureId),
  index: Venture.Index.t,
};

type action =
  | IndexLoaded(Venture.Index.t)
  | SelectVenture(ventureId);

let component = ReasonReact.reducerComponent("VentureList");

let selectVenture = e =>
  SelectVenture(
    ReactDOMRe.domElementToObj(ReactEventRe.Mouse.currentTarget(e))##getAttribute(
      "value",
    )
    |> VentureId.fromString,
  );

let make = (~session, _children) => {
  ...component,
  initialState: () => {status: LoadingIndex, index: [], selected: None},
  didMount: _self =>
    ReasonReact.SideEffects(
      ({send}) =>
        Js.Promise.(
          Venture.Index.load()
          |> then_(index => send(IndexLoaded(index)) |> resolve)
          |> ignore
        ),
    ),
  reducer: (action, state) =>
    switch (action) {
    | IndexLoaded(index) =>
      ReasonReact.Update({...state, status: None, index})
    | SelectVenture(id) =>
      Some(id) == state.selected ?
        ReasonReact.NoUpdate :
        ReasonReact.Update({...state, selected: Some(id)})
    /* ReasonReact.UpdateWithSideEffects( */
    /*   {...state, status: LoadingVenture, selected: None}, */
    /*   ( */
    /*     ({send}) => */
    /*       Js.Promise.( */
    /*         Venture.load(session, ~ventureId=id |> VentureId.fromString) */
    /*         |> then_(venture => send(VentureLoaded(venture)) |> resolve) */
    /*         |> ignore */
    /*       ) */
    /*   ), */
    /* ); */
    },
  render: ({send, state}) => {
    let ventureList =
      ReasonReact.arrayToElement(
        Array.of_list(
          (
            switch (state.status) {
            | LoadingIndex => []
            | _ => state.index
            }
          )
          |> List.map(
               Venture.Index.(
                 ({name, id}) =>
                   MaterialUi.(
                     <WithStyles
                       key=(id |> VentureId.toString)
                       classes=[
                         {
                           name: "selected",
                           styles:
                             ReactDOMRe.Style.make(
                               ~backgroundColor="gray",
                               (),
                             ),
                         },
                       ]
                       render={
                                let ids = id |> VentureId.toString;
                                classes =>
                                  <ListItem
                                    key=ids
                                    button=false
                                    value=(`String(ids))
                                    className=(
                                      Some(id) == state.selected ?
                                        classes##selected : ""
                                    )
                                    onClick=(e => send(selectVenture(e)))
                                    component=(`String("li"))>
                                    <ListItemText
                                      primary={
                                        <Router.Link route=(Venture(id))>
                                          (name |> ReasonReact.stringToElement)
                                        </Router.Link>
                                      }
                                    />
                                  </ListItem>;
                              }
                     />
                   )
               ),
             ),
        ),
      );
    let status =
      switch (state.status) {
      | LoadingIndex => ReasonReact.stringToElement("Loading Index")
      | _ => ReasonReact.stringToElement("ventures:")
      };
    MaterialUi.(
      <div>
        <Typography variant=`Title>
          (ReasonReact.stringToElement("My Ventures"))
        </Typography>
        <List> ventureList </List>
      </div>
    );
  },
};
