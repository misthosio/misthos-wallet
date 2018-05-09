[@bs.module] external logo : string = "../assets/img/logo-solid.svg";

[@bs.module] external menu : string = "../assets/img/menu.svg";

type action =
  | OpenDrawer
  | CloseDrawer;

type state = {drawerOpen: bool};

let component = ReasonReact.reducerComponent("Layout");

module Styles = {
  open Css;
  let flex_ = style([flex(1)]);
  let appBar =
    style([backgroundColor(Colors.white), boxShadow(Colors.white)]);
  let container =
    style([
      flexGrow(1),
      width(`percent(100.0)),
      height(`percent(100.0)),
      margin(px(0)),
      overflow(hidden),
    ]);
  let grid =
    style([
      width(`percent(100.0)),
      height(`calc((`sub, `percent(100.0), `px(64)))),
      margin(px(0)),
      overflowY(auto),
    ]);
  let drawer = style([width(`px(440)), flex(1)]);
};

let make = (~drawer, ~modal, children) => {
  ...component,
  initialState: () => {drawerOpen: false},
  reducer: (action, _state) =>
    switch (action) {
    | OpenDrawer => ReasonReact.Update({drawerOpen: true})
    | CloseDrawer => ReasonReact.Update({drawerOpen: false})
    },
  render: ({send, state}) =>
    MaterialUi.(
      <MuiThemeProvider
        theme=(`ObjectGeneric(Theme.theme |> Theme.toJsUnsafe))>
        <CssBaseline>
          <div className=Styles.container>
            (
              switch (drawer) {
              | None => ReasonReact.null
              | Some(drawer) =>
                <AppBar position=`Static className=Styles.appBar>
                  <Toolbar>
                    <img src=logo alt="logo" />
                    <div className=Styles.flex_ />
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
                    _open=state.drawerOpen>
                    <div
                      className=Styles.drawer
                      tabIndex=0
                      role="button"
                      onClick=(_event => send(CloseDrawer))>
                      drawer
                    </div>
                  </Drawer>
                </AppBar>
              }
            )
            (
              switch (modal) {
              | None => ReasonReact.null
              | Some((modal, onClose)) =>
                <Modal _open=true onClose>
                  <Paper>
                    <MButton onClick=onClose> ("X" |> Utils.text) </MButton>
                    modal
                  </Paper>
                </Modal>
              }
            )
            <Grid
              className=Styles.grid container=true spacing=V24 direction=`Row>
              children
            </Grid>
          </div>
        </CssBaseline>
      </MuiThemeProvider>
    ),
};
