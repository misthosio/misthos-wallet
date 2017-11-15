let component = ReasonReact.statelessComponent("SignIn");

let make = (_children) => {
  ...component,
  reducer: (newText, _text) => ReasonReact.Update(newText),
  render: (_self) =>
    <div className="panel-landing" id="section-1">
      <h1 className="landing-heading"> (ReasonReact.stringToElement("Welcome to Misthos")) </h1>
      <p className="lead">
        <button
          className="btn btn-primary btn-lg"
          id="signin-button"
          onClick=((_e) => Blockstack.redirectToSignIn())>
          (ReasonReact.stringToElement("Sign In with Blockstack"))
        </button>
      </p>
    </div>
};
