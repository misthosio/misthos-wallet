/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

[%bs.raw {|require('./assets/css/base.css')|}];

ReactDOMRe.renderToElementWithId(
  <JssProvider>
    <SessionStore>
      ...((~session, ~updateSession) => <App session updateSession />)
    </SessionStore>
  </JssProvider>,
  "root",
);
