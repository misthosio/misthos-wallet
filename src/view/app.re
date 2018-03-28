open PrimitiveTypes;

let text = ReasonReact.stringToElement;

type action =
  | LoginCompleted(Session.t)
  | SignIn
  | SignOut;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("App");

[%mui.withStyles
 "AppStyles"({
   flex: ReactDOMRe.Style.make(~flex="1", ()),
   container: ReactDOMRe.Style.make(~flexGrow="1", ~paddingTop="80px", ())
 })
];

let make = _children => {
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
  render: ({send, state}) => {
    let title = {
      let header = switch state.session {
        | NotLoggedIn => "Welcome To Misthos"
        | LoginPending => "Waiting for login to complete"
        | AnonymousLogin => "You must login with a registered blockstack id to use Misthos"
        | LoggedIn(data) => "Hello " ++ (data.userId |> UserId.toString)
      };
      <AppStyles
       render=(
         classes =>
           MaterialUi.(
             <Typography variant=`Title className=classes.flex>
               (ReasonReact.stringToElement(header))
               </Typography>
           )
       )/>
    };
    let loginButton =
      MaterialUi.(
        switch state.session {
          | NotLoggedIn =>
            <Button onClick=(_e => send(SignIn))>
            "Sign In with Blockstack"
            </Button>
          | LoggedIn(_) =>
            <Button onClick=(_e => send(SignOut))>
            "SignOut"
            </Button>
          | LoginPending
          | AnonymousLogin => <div />
        }
      );
    let ventures = {
      let ventures = session =>
        MaterialUi.(
        <Grid item=true xs=V12>
          <Paper>
            <Ventures session />
          </Paper>
        </Grid>);
      <AppStyles
        render=(
          classes =>
            MaterialUi.(
              <div className=classes.container>
                <Grid container=true spacing=V24>
                (
                  switch state.session {
                    | LoggedIn(session) => ventures(session)
                    | _ => <div />
                  }
                )
                </Grid>
                </div>
            )
        )/>;
    };
      MaterialUi.(
        <div>
          <CssBaseline/>
          <AppBar>
            <Toolbar>
            title
            loginButton
            </Toolbar>
          </AppBar>
          ventures
        </div>
      );
  }
};
