let component = ReasonReact.statelessComponent("TypographyStack");

let make = _children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid container=true>
        <Grid item=true xs=V6>
          <Typography variant=`Display4>
            ("Display 4" |> Utils.text)
          </Typography>
          <Typography variant=`Display3>
            ("Display 3" |> Utils.text)
          </Typography>
          <Typography variant=`Display2>
            ("Display 2" |> Utils.text)
          </Typography>
          <Typography variant=`Display1>
            ("Display 1" |> Utils.text)
          </Typography>
          <TitleBar>
            <Typography variant=`Headline>
              ("Headline" |> Utils.text)
            </Typography>
          </TitleBar>
          <Typography variant=`Title> ("Title" |> Utils.text) </Typography>
          <Typography variant=`Subheading>
            ("Subheading" |> Utils.text)
          </Typography>
          <Typography variant=`Body2> ("Body2" |> Utils.text) </Typography>
          <Typography variant=`Body1> ("Body1" |> Utils.text) </Typography>
          <Typography variant=`Caption> ("Caption" |> Utils.text) </Typography>
          <Typography variant=`Button> ("Button" |> Utils.text) </Typography>
        </Grid>
        <Grid item=true xs=V6>
          <MButton> ("Button" |> Utils.text) </MButton>
          <LinkButton route=TypographyStack>
            ("LinkButton" |> Utils.text)
          </LinkButton>
          <br />
          <Link route=TypographyStack> ("Link" |> Utils.text) </Link>
        </Grid>
      </Grid>
    ),
};
