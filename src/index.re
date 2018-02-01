[%bs.raw {|require('./assets/css/index.css')|}];

[%bs.raw {|require('bootstrap/dist/css/bootstrap.css')|}];

/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
ReactDOMRe.renderToElementWithId(
  <Router> ...(_currentUrl => <App /*currentUrl*/ />) </Router>,
  "root"
);

ReasonReact.Router.push(Location.pathname ++ Location.search);
