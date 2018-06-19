include ViewCommon;

let component = ReasonReact.statelessComponent("MisthosIs");

module Styles = {
  open Css;
  open BreakPoints;

  let grid =
    style([
      display(grid),
      xs([
        height(vh(95.0)),
        unsafe(
          "gridTemplateAreas",
          {|
           ". .    line .   ."
           ". text line img ."
           ". .    line .   ."
           |},
        ),
        unsafe("gridTemplateColumns", "0px 1fr 1px 1fr 0px"),
        unsafe("gridTemplateRows", "auto min-content auto"),
        gridGap(px(Theme.space(3))),
      ]),
    ]);

  let img =
    style([
      unsafe("gridArea", "img"),
      display(`flex),
      flexDirection(column),
      alignItems(center),
      children([unsafe("boxShadow", "2px 2px 0 0 #000000")]),
    ]);
  let text =
    style([
      unsafe("gridArea", "text"),
      display(`flex),
      flexDirection(column),
      justifyContent(center),
      alignItems(flexStart),
    ]);

  let display1 = style([]);
  let display2 = style([fontSize(px(62)), textTransform(uppercase)]);
  let display4 =
    style([
      fontSize(px(80)),
      unsafe("backgroundImage", Colors.uGradientAquaLight),
      unsafe("padding", "0px 16px"),
      unsafe("margin", "-34px 0px 26px -16px"),
      unsafe("zIndex", "-1"),
    ]);
  let line =
    style([unsafe("gridArea", "line"), backgroundColor(hex("1f2532"))]);
};

let make = (~primary, ~secondary, ~img, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <div className=Styles.grid>
        <div className=Styles.text>
          <Typography className=Styles.display2 variant=`Display2>
            ("Misthos is" |> text)
          </Typography>
          <Typography className=Styles.display4 variant=`Display4>
            (primary |> text)
          </Typography>
          <Typography className=Styles.display1 variant=`Display1>
            (secondary |> text)
          </Typography>
        </div>
        <div className=Styles.line />
        <div className=Styles.img> img </div>
      </div>
    ),
};
