let component = ReasonReact.statelessComponent("SignIn");

let make = (_children) => {
  ...component,
  render: (_self) =>
    <div className="panel-landing" id="section-1">
      <p className="lead">
        <button
          className="btn btn-primary btn-lg"
          id="signin-button"
          onClick=((_e) => Session.signOut())>
          (ReasonReact.stringToElement("SignOut"))
        </button>
      </p>
    </div>
};
