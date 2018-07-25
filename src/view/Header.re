include ViewCommon;

let component = ReasonReact.statelessComponent("Header");

module Styles = {
  open Css;
  let flex_ = style([flex(1)]);
  let appBar =
    style([backgroundColor(Colors.white), boxShadow(Colors.white)]);
  let logo =
    style([hover([backgroundColor(transparent)]), borderRadius(px(0))]);
};

let make = (~onClickLogo=?, ~hrefLogo=?, ~onClickMenu=?, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <AppBar position=`Static className=Styles.appBar>
        <Toolbar>
          {
            let (onClick, href) =
              switch (onClickLogo, hrefLogo) {
              | (Some(onClick), None) => (Some(onClick), None)
              | (_, Some(href)) => (None, Some(href))
              | (_, _) => (None, None)
              };
            switch (href) {
            | Some(href) => <a href> Icons.logoSolid </a>
            | None =>
              <IconButton
                className=Styles.logo
                color=`Inherit
                ?onClick
                disableRipple=true>
                Icons.logoSolid
              </IconButton>
            };
          }
          <div className=Styles.flex_ />
          (
            switch (onClickMenu) {
            | Some(onClick) =>
              <IconButton color=`Inherit onClick> Icons.menu </IconButton>
            | None => ReasonReact.null
            }
          )
        </Toolbar>
      </AppBar>
    ),
};
