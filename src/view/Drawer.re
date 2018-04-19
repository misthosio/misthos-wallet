let component = ReasonReact.statelessComponent("Drawer");

let make = (~onSignOut, ~index, ~selected=?, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classes=[
          {
            name: "container",
            styles:
              ReactDOMRe.Style.make(
                ~display="flex",
                ~flexDirection="column",
                (),
              ),
          },
          {name: "flex", styles: ReactDOMRe.Style.make(~flex="1", ())},
        ]
        render=(
          classes =>
            <div className=classes##container>
              <VentureList ?selected index />
              <div className=classes##flex />
              <LinkButton route=CreateVenture>
                ("Create a Venture" |> ReasonReact.stringToElement)
              </LinkButton>
              <div className=classes##flex />
              <Button color=`Inherit onClick=onSignOut> "Sign Out" </Button>
            </div>
        )
      />
    ),
};
