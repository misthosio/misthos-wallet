/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
ReactDOMRe.renderToElementWithId(
  <UserSession>
    ...((~session, ~updateSession) => <Router session updateSession />)
  </UserSession>,
  "root",
);
