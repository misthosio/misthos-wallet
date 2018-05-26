/* To access blockstack library from the browser */
[%bs.raw {|window.blockstack = require('blockstack')|}];

[%bs.raw {|require('./assets/css/base.css')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
ReactDOMRe.renderToElementWithId(<LoggedInHome />, "root");
