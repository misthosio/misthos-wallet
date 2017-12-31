[%bs.raw {|require('./index.css')|}];

/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
ReactDOMRe.renderToElementWithId(<App />, "root");
