open PrimitiveTypes;

[%bs.raw {|require('./app.css')|}];

let text = ReasonReact.stringToElement;

type action =
  | LoginCompleted(Session.t)
  | SignIn
  | SignOut;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("App");

let make = /*~currentUrl: ReasonReact.Router.url,*/ _children => {
  ...component,
  initialState: () => {session: Session.getCurrentSession()},
  didMount: ({state}) =>
    switch state.session {
    | LoginPending =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Session.completeLogIn()
              |> then_(session => send(LoginCompleted(session)) |> resolve)
              |> ignore
            )
        )
      )
    | _ => ReasonReact.NoUpdate
    },
  reducer: (action, _state) =>
    switch action {
    | LoginCompleted(session) => ReasonReact.Update({session: session})
    | SignIn => ReasonReact.Update({session: Session.signIn()})
    | SignOut => ReasonReact.Update({session: Session.signOut()})
    },
  render: ({send, state}) =>
    <div className="wrapper">
      /* <Sidebar currentUrl /> */
      /* <div id="main-panel" className="main-panel"> <Header /> </div> */

        <div>
          {
            let header =
              switch state.session {
              | NotLoggedIn => "Welcome To Misthos"
              | LoginPending => "Waiting for login to complete"
              | AnonymousLogin => "You must login with a registered blockstack id to use Misthos"
              | LoggedIn(data) => "Hello " ++ (data.userId |> UserId.toString)
              };
            <h1> (text(header)) </h1>;
          }
          (
            switch state.session {
            | NotLoggedIn =>
              <button onClick=(_e => send(SignIn))>
                (text("Sign In with Blockstack"))
              </button>
            | LoggedIn(_) =>
              <button onClick=(_e => send(SignOut))>
                (text("SignOut"))
              </button>
            | LoginPending
            | AnonymousLogin => <div />
            }
          )
          (
            switch state.session {
            | LoggedIn(session) => <Ventures session />
            | _ => <div />
            }
          )
        </div>
      </div>
};
