let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, ~updateVenture, _children) => {
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
                  <SelectedVenture venture updateVenture session />
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
