let component = ReasonReact.statelessComponent("Spinner");

let make = (~text, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classes=[
          {
            name: "progress",
            styles: ReactDOMRe.Style.make(~color="#02a2b4", ()),
          },
          {
            name: "container",
            styles: ReactDOMRe.Style.make(~textAlign="center", ()),
          },
        ]
        render=(
          classes =>
            <Grid
              container=true direction=`Row alignItems=`Center justify=`Center>
              <Grid container=true direction=`Row justify=`Center>
                <Grid className=classes##container item=true xs=V8 md=V4 lg=V2>
                  <CircularProgress className=classes##progress />
                  <Typography variant=`Body1>
                    (text |> ReasonReact.stringToElement)
                  </Typography>
                </Grid>
              </Grid>
            </Grid>
        )
      />
    ),
};
