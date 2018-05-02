let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, _children) => {
  ...component,
  render: _self =>
    switch ((selectedVenture: VentureStore.workerizedVentureState)) {
    | VentureLoaded(venture, commands) =>
      <MaterialUi.Grid item=true xs=V12>
        <SelectedVenture venture commands session />
      </MaterialUi.Grid>
    | JoiningVenture => <Spinner text="Joining venture" />
    | LoadingVenture => <Spinner text="Loading venture" />
    | CreatingVenture => <Spinner text="Creating venture" />
    | None => Utils.text("Not selected")
    },
};
