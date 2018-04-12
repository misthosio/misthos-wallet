let text = ReasonReact.stringToElement;

type action =
  | OpenDrawer
  | CloseDrawer;

type state = {_open: bool};

let component = ReasonReact.reducerComponent("Layout");

[%mui.withStyles
  "LayoutStyles"({
    flex: ReactDOMRe.Style.make(~flex="1", ()),
    container: ReactDOMRe.Style.make(~flexGrow="1", ~paddingTop="80px", ()),
    drawer: ReactDOMRe.Style.make(~width="250px", ~flex="1", ()),
  })
];

let logo =
  <MaterialUi.SvgIcon color=`Inherit viewBox="0 -10 50 50">
    <path
      d="M18.5987902,15.077098 L27.4008089,15.077098 L27.4008089,33 L18.5987902,33 L18.5987902,15.077098 Z M32.3625445,0.00114787781 C39.8711038,0.0111907447 46,6.44706158 46,14.3889607 L46,33 L37.1979813,33 L37.1979813,14.3889607 C37.1979813,11.4134601 35.0296527,8.97786404 32.2145945,8.97786404 C30.1104177,8.97786404 28.41601,10.2404533 27.6393731,12.2321546 L18.3181264,12.2321546 C17.5418904,10.2404533 15.8683321,8.97786404 13.7641552,8.97786404 C10.9498989,8.97786404 8.80241964,11.4138618 8.80241964,14.3889607 L8.80241964,14.5653135 L8.80241964,33 L0,33 L0,0.283151581 L8.80241964,0.283151581 L8.80241964,1.04640947 C13.6298375,-1.06178916 19.267171,0.0706445169 23.1299073,3.94638772 C25.6514713,1.41116639 28.8827139,-0.00166412493 32.3625445,0.00114787781 Z"
    />
  </MaterialUi.SvgIcon>;

let make = (~drawer, children) => {
  ...component,
  initialState: () => {_open: false},
  reducer: (action, _state) =>
    switch (action) {
    | OpenDrawer => ReasonReact.Update({_open: true})
    | CloseDrawer => ReasonReact.Update({_open: false})
    },
  render: ({send, state}) =>
    MaterialUi.(
      <LayoutStyles
        render=(
          classes =>
            <MuiThemeProvider
              theme=(`ObjectGeneric(Theme.theme |> Theme.toJsUnsafe))>
              <CssBaseline />
              <AppBar>
                <Toolbar>
                  logo
                  <div className=classes.flex />
                  (
                    switch (drawer) {
                    | None => <div />
                    | Some(_) =>
                      <IconButton
                        color=`Inherit onClick=(_e => send(OpenDrawer))>
                        <MaterialUIIcons.Menu />
                      </IconButton>
                    }
                  )
                </Toolbar>
              </AppBar>
              (
                switch (drawer) {
                | None => <div />
                | Some(drawer) =>
                  <Drawer
                    theme=(Theme.theme |> Theme.toJsUnsafe)
                    variant=`Temporary
                    anchor=`Right
                    onClose=(() => send(CloseDrawer))
                    _open=state._open>
                    <div
                      className=classes.drawer
                      tabIndex=0
                      role="button"
                      onClick=(_event => send(CloseDrawer))>
                      drawer
                    </div>
                  </Drawer>
                }
              )
              <div className=classes.container>
                <Grid container=true spacing=V24> children </Grid>
              </div>
            </MuiThemeProvider>
        )
      />
    ),
};
