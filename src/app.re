[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let component = ReasonReact.statelessComponent("App");

let make = (~session: Session.session, _children) => {
  ...component,
  render: (_self) =>
    <div className="site-wrapper">
      {
        let header =
          switch session {
          | NotLoggedIn => "Welcome To Misthos"
          | LoggedIn(user) => "Hello " ++ user.userName
          };
        <h1 className="landing-heading"> (ReasonReact.stringToElement(header)) </h1>
      }
      <div className="site-wrapper-inner">
        (
          switch session {
          | NotLoggedIn => <SignIn />
          | LoggedIn(_) => <SignOut />
          }
        )
      </div>
    </div>
};
