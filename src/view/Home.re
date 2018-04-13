let component = ReasonReact.statelessComponent("Home");

let make = (~session, _children) => {
  ...component,
  render: _self =>
    <MaterialUi.WithStyles
      render=(
        _classes =>
          MaterialUi.(
            <Grid item=true xs=V12>
              <Paper> <Ventures session /> </Paper>
            </Grid>
          )
      )
    />,
};
