open PrimitiveTypes;

let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, ~updateVentureStore, _children) => {
  /* TODO handle this via routing (and remove need for ~updateVentureStore) */
  let onCreateVenture = name =>
    updateVentureStore(VentureStore.CreateVenture(session, name));
  {
    ...component,
    render: _self =>
      <MaterialUi.WithStyles
        render=(
          _classes =>
            MaterialUi.(
              <Grid item=true xs=V12>
                <CreateVenture onCreateVenture />
                (
                  switch ((selectedVenture: VentureStore.ventureState)) {
                  | VentureLoaded(venture) =>
                    <SelectedVenture
                      key=(venture |> Venture.getId |> VentureId.toString)
                      venture
                      session
                    />
                  | JoiningVenture =>
                    ReasonReact.stringToElement("Joining venture")
                  | LoadingVenture =>
                    ReasonReact.stringToElement("Loading venture")
                  | CreatingVenture =>
                    ReasonReact.stringToElement("Creating venture")
                  | None => ReasonReact.stringToElement("Not selected")
                  }
                )
              </Grid>
            )
        )
      />,
  };
};
