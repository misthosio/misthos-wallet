let component = ReasonReact.statelessComponent("Spinner");

module Styles = {
  open Css;
  let progress = style([color(Colors.misthosTeal)]);
  let container = style([textAlign(center)]);
};

let make = (~text, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid container=true direction=`Row alignItems=`Center justify=`Center>
        <Grid container=true direction=`Row justify=`Center>
          <Grid className=Styles.container item=true xs=V8 md=V4 lg=V2>
            <CircularProgress className=Styles.progress />
            <Typography variant=`Body1> (text |> Utils.text) </Typography>
          </Grid>
        </Grid>
      </Grid>
    ),
};
