include ViewCommon;

let component = ReasonReact.statelessComponent("MisthosIs");

module Styles = {
  open Css;
  open BreakPoints;

  let arrowIcon = Icons.asDataUrl(Icons.arrowDownBig);

  let grid =
    style([
      height(vh(95.0)),
      md([
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
        display(grid),
      ]),
      xs([
        unsafe(
          "gridTemplateAreas",
          {|
           ". .    ."
           ". text ."
           ". img  ."
           ". .    ."
           |},
        ),
        unsafe("gridTemplateColumns", "0px 1fr 0px"),
        unsafe("gridTemplateRows", "auto min-content min-content auto"),
        gridGap(px(Theme.space(1))),
        display(none),
      ]),
      backgroundImage(url(arrowIcon)),
      backgroundRepeat(noRepeat),
      unsafe("backgroundPosition", "top, center"),
    ]);

  let last =
    style([
      unsafe("backgroundImage", {j|url($arrowIcon), url($arrowIcon)|j}),
      unsafe("backgroundPosition", "top center, bottom 24px center"),
      unsafe("backgroundRepeat", "no-repeat, no-repeat"),
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
      lg([fontSize(px(80))]),
      xs([fontSize(px(65))]),
      unsafe("backgroundImage", Colors.uGradientAquaLight),
      unsafe("padding", "0px 16px"),
      unsafe("margin", "-34px 0px 26px -16px"),
      unsafe("zIndex", "-1"),
    ]);
  let line = last =>
    style([
      unsafe("gridArea", "line"),
      marginTop(px(45)),
      marginBottom(px(last ? 69 : 0)),
      backgroundColor(hex("1f2532")),
      display(none),
      md([display(block)]),
    ]);
};

let make = (~primary, ~secondary, ~img, ~last=false, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <div className=(Styles.grid ++ " " ++ (last ? Styles.last : ""))>
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
        <div className=(Styles.line(last)) />
        <div className=Styles.img> img </div>
      </div>
    ),
};
