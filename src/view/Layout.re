type action =
  | OpenDrawer
  | CloseDrawer;

type state = {drawerOpen: bool};

let component = ReasonReact.reducerComponent("Layout");

module Styles = {
  open Css;
  let flex_ = style([flex(1)]);
  let appBar =
    style([
      backgroundColor(Colors.white),
      boxShadow(Colors.white),
      unsafe("gridArea", "bar"),
    ]);
  let body = style([minHeight(px(0)), unsafe("gridArea", "body")]);
  let gap = (Theme.space(8) |> string_of_int) ++ "px";
  let grid = mobileEnabled =>
    style([
      display(grid),
      minWidth(mobileEnabled ? px(0) : px(Theme.space(101))),
      minHeight(mobileEnabled ? px(0) : px(Theme.space(88))),
      height(vh(100.0)),
      unsafe("gridTemplateColumns", "[begin] 1fr [end]"),
      unsafe("gridTemplateRows", {j|[begin] min-content 1fr $gap [end]|j}),
      unsafe("gridTemplateAreas", {|"bar" "body" "."|}),
    ]);
  let drawer = style([width(`px(440)), height(`percent(100.0))]);
  let drawerPaper = style([height(`percent(100.0))]);
  let modalContent =
    style([
      height(`calc((`sub, `percent(100.0), `px(64)))),
      paddingBottom(px(Theme.space(8))),
    ]);
  let modal =
    style([
      BreakPoints.md([
        width(`vw(90.0)),
        height(`vh(90.0)),
        margin2(~v=`vh(5.0), ~h=`vw(5.0)),
      ]),
      width(`percent(100.0)),
      height(`percent(100.0)),
      focus([outlineStyle(`none)]),
    ]);
  let logo =
    style([hover([backgroundColor(transparent)]), borderRadius(px(0))]);
};

let make = (~drawer, ~modal, ~mobileEnabled=false, children) => {
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
                   <div className=Styles.modalContent> modal </div>
                 </Paper>,
                 ~props={"id": "modal"},
                 [||],
               );
             <Modal _open=true onBackdropClick=onClose> inner </Modal>;
           })
        |> Js.Option.getWithDefault(ReasonReact.null)
      );
    MaterialUi.(
      <div className=(Styles.grid(mobileEnabled))>
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
                <IconButton color=`Inherit onClick=(_e => send(OpenDrawer))>
                  Icons.menu
                </IconButton>
              </Toolbar>
              <Drawer
                theme
                classes=[Paper(Styles.drawerPaper)]
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
        (
          ReasonReact.createDomElement(
            "div",
            ~props={"className": Styles.body},
            children,
          )
        )
      </div>
    );
  },
};
