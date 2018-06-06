type action =
  | OpenDrawer
  | CloseDrawer;

type state = {drawerOpen: bool};

let component = ReasonReact.reducerComponent("Layout");

module Styles = {
  open Css;
  global("html, body, #root", [height(`percent(100.0))]);
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
  let modal =
    style([
      media(
        "(min-width: 960px)",
        [
          width(`vw(90.0)),
          height(`vh(90.0)),
          margin2(~v=`vh(5.0), ~h=`vw(5.0)),
        ],
      ),
      width(`percent(100.0)),
      height(`percent(100.0)),
      focus([outlineStyle(`none)]),
    ]);
  let logo =
    style([hover([backgroundColor(transparent)]), borderRadius(px(0))]);
};

let make = (~drawer, ~modal, children) => {
  ...component,
  initialState: () => {drawerOpen: false},
  reducer: (action, _state) =>
    switch (action) {
    | OpenDrawer => ReasonReact.Update({drawerOpen: true})
    | CloseDrawer => ReasonReact.Update({drawerOpen: false})
    },
  render: ({send, state}) => {
    let theme = Theme.theme |> Theme.toJsUnsafe;
    let modalContainer =
      MaterialUi.(
        modal
        |> Utils.mapOption(((modal, onClose)) => {
             /* We need to add the id #modal to the focused element for the clipboard to work */
             /* in the Receive modal */
             let inner =
               ReasonReact.cloneElement(
                 <Paper className=Styles.modal>
                   <Toolbar>
                     <div className=Styles.flex_ />
                     <IconButton color=`Inherit onClick=onClose>
                       Icons.close
                     </IconButton>
                   </Toolbar>
                   modal
                 </Paper>,
                 ~props={"id": "modal"},
                 [||],
               );
             <Modal _open=true onBackdropClick=onClose> inner </Modal>;
           })
        |> Js.Option.getWithDefault(ReasonReact.null)
      );
    MaterialUi.(
      <MuiThemeProvider theme=(`ObjectGeneric(theme))>
        <CssBaseline>
          <div className=Styles.container>
            (
              switch (drawer) {
              | None => ReasonReact.null
              | Some(drawer) =>
                <AppBar position=`Static className=Styles.appBar>
                  <Toolbar>
                    <IconButton
                      className=Styles.logo
                      color=`Inherit
                      onClick=(Router.clickToRoute(Home))>
                      Icons.logoSolid
                    </IconButton>
                    <div className=Styles.flex_ />
                    <IconButton
                      color=`Inherit onClick=(_e => send(OpenDrawer))>
                      Icons.menu
                    </IconButton>
                  </Toolbar>
                  <Drawer
                    theme
                    variant=`Temporary
                    anchor=`Right
                    onClose=(_ => send(CloseDrawer))
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
            modalContainer
            <Grid
              className=Styles.grid container=true spacing=V24 direction=`Row>
              children
            </Grid>
          </div>
        </CssBaseline>
      </MuiThemeProvider>
    );
  },
};
