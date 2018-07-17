[%bs.raw {|window.blockstack = require('blockstack')|}];

let theme = Theme.theme() |> Theme.toJsUnsafe;

ReactDOMRe.renderToElementWithId(
  <JssProvider>
    <SessionStore>
      ...(
           (~session, ~updateSession, ~signTAC) =>
             MaterialUi.(
               <MuiThemeProvider theme=(`ObjectGeneric(theme))>
                 <CssBaseline>
                   <App session updateSession signTAC />
                 </CssBaseline>
               </MuiThemeProvider>
             )
         )
    </SessionStore>
  </JssProvider>,
  "root",
);
