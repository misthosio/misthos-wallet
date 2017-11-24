let component = ReasonReact.statelessComponent("SignIn");

let make = (_children) => {
  ...component,
  render: (_self) =>
    <button
      className="btn btn-primary btn-lg" id="signin-button" onClick=((_e) => Session.signIn())>
      (ReasonReact.stringToElement("Sign In with Blockstack"))
    </button>
};
