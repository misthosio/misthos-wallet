let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture=?, _children) => {
  ...component,
  render: _self =>
    <MaterialUi.WithStyles
      render=(
        _classes =>
          MaterialUi.(
            <Grid item=true xs=V12>
              <Paper>
                (
                  switch (selectedVenture) {
                  | Some(venture) =>
                    <SelectedVenture
                      key=(venture |> Venture.getId)
                      venture
                      session
                    />
                  | None => ReasonReact.stringToElement("Venture loading")
                  }
                )
              </Paper>
            </Grid>
          )
      )
    />,
};
