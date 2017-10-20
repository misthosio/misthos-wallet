[%bs.raw {|require('./index.css')|}];

/* external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"]; */
let userData =
  Blockstack.isUserSignedIn () ?
    Blockstack.loadUserData () :
    {
      if (Blockstack.isSignInPending ()) {
        Blockstack.handlePendingSignIn ()
      };
      None
    };

ReactDOMRe.renderToElementWithId <App userData /> "root";
/* register_service_worker (); */
