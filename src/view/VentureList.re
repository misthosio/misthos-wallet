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

let make = (~session, ~selected=?, _children) => {
  ...component,
  initialState: () => {status: LoadingIndex, index: [], selected},
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
                         {
                           name: "link",
                           styles:
                             ReactDOMRe.Style.make(
                               ~display="block",
                               ~width="100%",
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
                                        <Link
                                          className=classes##link
                                          route=(Venture(id))>
                                          (name |> ReasonReact.stringToElement)
                                        </Link>
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
