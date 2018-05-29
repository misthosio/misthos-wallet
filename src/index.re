[%bs.raw {|window.blockstack = require('blockstack')|}];

ReactDOMRe.renderToElementWithId(
  <JssProvider>
    <SessionStore>
      ...((~session, ~updateSession) => <App session updateSession />)
    </SessionStore>
  </JssProvider>,
  "root",
);
