let component = ReasonReact.statelessComponent("Home");

let make = (~session, ~selectedVenture, _children) => {
  ...component,
  render: _self =>
    switch ((selectedVenture: VentureStore.selectedVenture)) {
    | VentureLoaded(_ventureId, venture, commands) =>
      <SelectedVenture venture commands session />
    | JoiningVenture(_) => <Spinner text="Joining venture" />
    | LoadingVenture(_) => <Spinner text="Loading venture" />
    | CreatingVenture => <Spinner text="Creating venture" />
    | None => Utils.text("Not selected")
    },
};
