[%bs.raw {|require('./assets/css/index.css')|}];

[%bs.raw {|require('bootstrap/dist/css/bootstrap.css')|}];

/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

[%bs.raw {|window.d3 = require('d3')|}];

[%bs.raw {|window.bigi = require('bigi')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
ReactDOMRe.renderToElementWithId(
  <Router> ...(_currentUrl => <App /*currentUrl*/ />) </Router>,
  "root"
);
