let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, ~updateVenture, _children) => {
  ...component,
  render: _self =>
    switch ((selectedVenture: VentureStore.ventureState)) {
    | VentureLoaded(venture) =>
      <MaterialUi.Grid item=true xs=V12>
        <SelectedVenture venture updateVenture session />
      </MaterialUi.Grid>
    | JoiningVenture => <Spinner text="Joining venture" />
    | LoadingVenture => <Spinner text="Loading venture" />
    | CreatingVenture => <Spinner text="Creating venture" />
    | None => Utils.text("Not selected")
    },
};
