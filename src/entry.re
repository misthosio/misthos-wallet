[%bs.raw {|window.blockstack = require('blockstack')|}];

let theme = Theme.theme |> Theme.toJsUnsafe;

ReactDOMRe.renderToElementWithId(
  <JssProvider>
    <SessionStore>
      ...(
           (~session, ~updateSession) =>
             MaterialUi.(
               <MuiThemeProvider theme=(`ObjectGeneric(theme))>
                 <CssBaseline> <App session updateSession /> </CssBaseline>
               </MuiThemeProvider>
             )
         )
    </SessionStore>
  </JssProvider>,
  "root",
);
