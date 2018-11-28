let component = ReasonReact.statelessComponent("PageLayout");

let theme = Theme.theme() |> Theme.toJsUnsafe;

let make = children => {
  ...component,
  render: _self =>
    <JssProvider>
      MaterialUi.(
        <MuiThemeProvider theme={`ObjectGeneric(theme)}>
          <CssBaseline> children </CssBaseline>
        </MuiThemeProvider>
      )
    </JssProvider>,
};
