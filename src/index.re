[%bs.raw {|require('./index.css')|}];

/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
let session = Session.getCurrentSession();

ReactDOMRe.renderToElementWithId(<App session />, "root");
