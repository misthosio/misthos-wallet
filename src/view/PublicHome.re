[@bs.module] external logo : string = "../assets/img/logo-big.svg";

let text = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent("PublicHome");

let make = (~onSignIn, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classes=[
          {name: "flex", styles: ReactDOMRe.Style.make(~flex="1", ())},
          {
            name: "display4",
            styles: ReactDOMRe.Style.make(~paddingBottom="1.5vw", ()),
          },
          {
            name: "background",
            styles:
              ReactDOMRe.Style.make(
                ~backgroundImage="url(" ++ logo ++ ")",
                ~backgroundRepeat="no-repeat",
                ~backgroundSize="40vw",
                (),
              ),
          },
        ]
        render=(
          classes =>
            <Grid
              className=classes##background
              container=true
              direction=`Row
              alignItems=`Center
              justify=`Center>
              <Grid container=true direction=`Row justify=`Center>
                <Grid item=true xs=V10>
                  <Typography variant=`Display4>
                    ("Distribute Funds" |> text)
                  </Typography>
                  <Typography className=classes##display4 variant=`Display4>
                    ("with misthos." |> text)
                  </Typography>
                </Grid>
                <Grid xs=V10 container=true justify=`Space_Between>
                  <Grid item=true xs=V8>
                    <Typography variant=`Display1>
                      (
                        "Misthos is the only multi-sig Bitcoin wallet that lets you change co-singers in a fast and friction-less way."
                        |> text
                      )
                    </Typography>
                    <br />
                    <Typography variant=`Display1>
                      ("Use if for projects. Use if for payments." |> text)
                    </Typography>
                  </Grid>
                  <Grid container=true direction=`Column xs=V4>
                    <div className=classes##flex />
                    <Button color=`Inherit onClick=onSignIn>
                      "Sign In with Blockstack"
                    </Button>
                  </Grid>
                </Grid>
              </Grid>
            </Grid>
        )
      />
    ),
};
