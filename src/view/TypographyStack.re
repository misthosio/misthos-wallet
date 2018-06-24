include ViewCommon;

let component = ReasonReact.statelessComponent("TypographyStack");

let make = _children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid container=true>
        <Grid item=true xs=`V6>
          <Typography variant=`Display4> ("Display 4" |> text) </Typography>
          <Typography variant=`Display3> ("Display 3" |> text) </Typography>
          <Typography variant=`Display2> ("Display 2" |> text) </Typography>
          <Typography variant=`Display1> ("Display 1" |> text) </Typography>
          <Typography variant=`Title> ("Title" |> text) </Typography>
          <Typography variant=`Subheading> ("Subheading" |> text) </Typography>
          <Typography variant=`Body2> ("Body2" |> text) </Typography>
          <Typography variant=`Body1> ("Body1" |> text) </Typography>
          <Typography variant=`Caption> ("Caption" |> text) </Typography>
          <Typography variant=`Button> ("Button" |> text) </Typography>
        </Grid>
        <Grid item=true xs=`V6>
          <MButton> ("Button" |> text) </MButton>
          <br />
          <Link route=TypographyStack> ("Link" |> text) </Link>
        </Grid>
      </Grid>
    ),
};
