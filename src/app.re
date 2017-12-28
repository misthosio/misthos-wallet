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
          | LoginPending => "Logging in"
          };
        <h1 className="landing-heading"> (ReasonReact.stringToElement(header)) </h1>
      }
      (
        switch session {
        | NotLoggedIn => <SignIn />
        | LoggedIn(_) => <SignOut />
        | LoginPending => <div />
        }
      )
      (
        switch session {
        | LoggedIn(_) => <Projects />
        | _ => <div />
        }
      )
    </div>
};
