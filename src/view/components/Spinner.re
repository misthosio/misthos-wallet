include ViewCommon;

let component = ReasonReact.statelessComponent("Spinner");

module Styles = {
  open Css;
  let progress = style([color(Colors.misthosTeal)]);
  let container = style([textAlign(center), height(`percent(100.0))]);
};

let make = (~text as spinnerText, ~className="", _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Grid
        container=true
        className=(Styles.container ++ " " ++ className)
        direction=`Row
        alignItems=`Center
        justify=`Center>
        <Grid container=true direction=`Row justify=`Center>
          <Grid className=Styles.container item=true xs=`V8 md=`V4 lg=`V2>
            <CircularProgress className=Styles.progress />
            <Typography variant=`Body1> (spinnerText |> text) </Typography>
          </Grid>
        </Grid>
      </Grid>
    ),
};
