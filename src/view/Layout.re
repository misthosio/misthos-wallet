let text = ReasonReact.stringToElement;

[@bs.module] external logo : string = "../assets/img/logo-solid.svg";

[@bs.module] external menu : string = "../assets/img/menu.svg";

type action =
  | OpenDrawer
  | CloseDrawer;

type state = {open_: bool};

let component = ReasonReact.reducerComponent("Layout");

let make = (~drawer, children) => {
  ...component,
  initialState: () => {open_: false},
  reducer: (action, _state) =>
    switch (action) {
    | OpenDrawer => ReasonReact.Update({open_: true})
    | CloseDrawer => ReasonReact.Update({open_: false})
    },
  render: ({send, state}) =>
    MaterialUi.(
      <WithStyles
        classes=[
          {name: "flex", styles: ReactDOMRe.Style.make(~flex="1", ())},
          {
            name: "appBar",
            styles:
              ReactDOMRe.Style.make(
                ~backgroundColor="#FFFFFF",
                ~boxShadow="none",
                (),
              ),
          },
          {
            name: "container",
            styles:
              ReactDOMRe.Style.make(
                ~flexGrow="1",
                ~width="100%",
                ~height="100%",
                ~margin="0",
                ~overflow="hidden",
                (),
              ),
          },
          {
            name: "grid",
            styles:
              ReactDOMRe.Style.make(
                ~width="100%",
                ~height="calc(100% - (64px))",
                ~margin="0",
                ~overflowY="auto",
                (),
              ),
          },
          {
            name: "drawer",
            styles: ReactDOMRe.Style.make(~width="20vw", ~flex="1", ()),
          },
        ]
        render=(
          classes =>
            <MuiThemeProvider
              theme=(`ObjectGeneric(Theme.theme |> Theme.toJsUnsafe))>
              <CssBaseline>
                <div className=classes##container>
                  (
                    switch (drawer) {
                    | None => <div />
                    | Some(drawer) =>
                      <AppBar position=`Static className=classes##appBar>
                        <Toolbar>
                          <img src=logo alt="logo" />
                          <div className=classes##flex />
                          <IconButton
                            color=`Inherit onClick=(_e => send(OpenDrawer))>
                            <img src=menu alt="menu" />
                          </IconButton>
                        </Toolbar>
                        <Drawer
                          theme=(Theme.theme |> Theme.toJsUnsafe)
                          variant=`Temporary
                          anchor=`Right
                          onClose=(() => send(CloseDrawer))
                          _open=state.open_>
                          <div
                            className=classes##drawer
                            tabIndex=0
                            role="button"
                            onClick=(_event => send(CloseDrawer))>
                            drawer
                          </div>
                        </Drawer>
                      </AppBar>
                    }
                  )
                  <Grid
                    className=classes##grid
                    container=true
                    spacing=V24
                    direction=`Row>
                    children
                  </Grid>
                </div>
              </CssBaseline>
            </MuiThemeProvider>
        )
      />
    ),
};
