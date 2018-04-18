open PrimitiveTypes;

let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, _children) => {
  ...component,
  render: _self =>
    <MaterialUi.WithStyles
      render=(
        _classes =>
          MaterialUi.(
            <Grid item=true xs=V12>
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
