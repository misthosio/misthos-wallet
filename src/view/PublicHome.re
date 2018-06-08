include ViewCommon;

let component = ReasonReact.statelessComponent("PublicHome");

module Styles = {
  open Css;
  let fullHeight = style([height(`percent(100.0))]);
  let display4 = style([paddingBottom(`vw(1.5))]);
  let background =
    style([
      backgroundImage(url(Icons.asDataUrl(Icons.logoBig))),
      backgroundRepeat(noRepeat),
      backgroundSize(`size((`px(584), `px(419)))),
    ]);
};

let make = (~onSignIn, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid
        className=Styles.background
        container=true
        direction=`Row
        alignItems=`Center
        justify=`Center>
        <Grid container=true direction=`Row justify=`Center>
          <Grid item=true xs=V10>
            <Typography variant=`Display4>
              ("Distribute Funds" |> text)
            </Typography>
            <Typography className=Styles.display4 variant=`Display4>
              ("with misthos." |> text)
            </Typography>
          </Grid>
          <Grid item=true xs=V10>
            <Grid container=true justify=`Space_Between>
              <Grid item=true xs=V8>
                <Typography variant=`Display1>
                  (
                    "Misthos is the only multi-sig Bitcoin wallet that lets you change co-singers in a fast and friction-less way."
                    |> text
                  )
                </Typography>
                <br />
                <Typography variant=`Display1>
                  ("Use it for projects. Use it for payments." |> text)
                </Typography>
              </Grid>
              <Grid item=true xs=V3>
                <Grid
                  container=true
                  className=Styles.fullHeight
                  alignItems=`Flex_End>
                  <MButton color=`Inherit onClick=onSignIn>
                    "Sign In with Blockstack"
                  </MButton>
                </Grid>
              </Grid>
            </Grid>
          </Grid>
        </Grid>
      </Grid>
    ),
};
