[%bs.raw {|require('./index.css')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
let session = Session.getCurrentSession();

ReactDOMRe.renderToElementWithId(<App session />, "root");
/* register_service_worker (); */
