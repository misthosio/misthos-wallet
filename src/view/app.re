open PrimitiveTypes;

let text = ReasonReact.stringToElement;

type action =
  | LoginCompleted(Session.t)
  | SignIn
  | SignOut
  | OpenDrawer
  | CloseDrawer;

type state = {
  session: Session.t,
  drawer: bool,
};

let component = ReasonReact.reducerComponent("App");

[%mui.withStyles
  "AppStyles"({
    flex: ReactDOMRe.Style.make(~flex="1", ()),
    container: ReactDOMRe.Style.make(~flexGrow="1", ~paddingTop="80px", ()),
    drawer: ReactDOMRe.Style.make(~width="250px", ~flex="1", ()),
  })
];

let make = _children => {
  ...component,
  initialState: () => {session: Session.getCurrentSession(), drawer: false},
  didMount: ({state}) =>
    switch (state.session) {
    | LoginPending =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Session.completeLogIn()
              |> then_(session => send(LoginCompleted(session)) |> resolve)
              |> ignore
            )
        ),
      )
    | _ => ReasonReact.NoUpdate
    },
  reducer: (action, state) =>
    switch (action) {
    | LoginCompleted(session) => ReasonReact.Update({...state, session})
    | SignIn => ReasonReact.Update({...state, session: Session.signIn()})
    | SignOut => ReasonReact.Update({...state, session: Session.signOut()})
    | OpenDrawer => ReasonReact.Update({...state, drawer: true})
    | CloseDrawer => ReasonReact.Update({...state, drawer: false})
    },
  render: ({send, state}) => {
    let title = {
      let header =
        switch (state.session) {
        | NotLoggedIn => "Welcome To Misthos"
        | LoginPending => "Waiting for login to complete"
        | AnonymousLogin => "You must login with a registered blockstack id to use Misthos"
        | LoggedIn(data) => "Hello " ++ (data.userId |> UserId.toString)
        };
      <AppStyles
        render=(
          classes =>
            MaterialUi.(
              <Typography color=`Inherit variant=`Title className=classes.flex>
                (ReasonReact.stringToElement(header))
              </Typography>
            )
        )
      />;
    };
    let loginButton =
      MaterialUi.(
        switch (state.session) {
        | NotLoggedIn =>
          <Button color=`Inherit onClick=(_e => send(SignIn))>
            "Sign In with Blockstack"
          </Button>
        | LoggedIn(_) =>
          <Button color=`Inherit onClick=(_e => send(SignOut))>
            "SignOut"
          </Button>
        | LoginPending
        | AnonymousLogin => <div />
        }
      );
    let drawerButton =
      MaterialUi.(
        <IconButton color=`Inherit onClick=(_e => send(OpenDrawer))>
          <MaterialUIIcons.Menu />
        </IconButton>
      );
    let ventures = {
      let ventures = session =>
        MaterialUi.(
          <Grid item=true xs=V12> <Paper> <Ventures session /> </Paper> </Grid>
        );
      <AppStyles
        render=(
          classes =>
            MaterialUi.(
              <div className=classes.container>
                <Grid container=true spacing=V24>
                  (
                    switch (state.session) {
                    | LoggedIn(session) => ventures(session)
                    | _ => <div />
                    }
                  )
                </Grid>
              </div>
            )
        )
      />;
    };
    MaterialUi.(
      <MuiThemeProvider theme=(`ObjectGeneric(Theme.theme))>
        <CssBaseline />
        <AppBar> <Toolbar> title loginButton drawerButton </Toolbar> </AppBar>
        <AppStyles
          render=(
            classes =>
              <Drawer
                theme=Theme.theme
                variant=`Temporary
                anchor=`Right
                onClose=(() => send(CloseDrawer))
                _open=state.drawer>
                <div
                  className=classes.drawer
                  tabIndex=0
                  role="button"
                  onClick=(_event => send(CloseDrawer))
                  onKeyDown=(_event => send(CloseDrawer))>
                  <p> (ReasonReact.stringToElement("Venture list")) </p>
                </div>
              </Drawer>
          )
        />
        ventures
      </MuiThemeProvider>
    );
  },
};
