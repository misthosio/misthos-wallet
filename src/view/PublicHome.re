include ViewCommon;

let component = ReasonReact.statelessComponent("PublicHome");

module Styles = {
  open Css;
  open BreakPoints;

  let grid =
    style([
      display(grid),
      lg([
        unsafe(
          "gridTemplateAreas",
          {|
           ". . . ."
           ". title title ."
           ". sub button ."
           ". . . ."
           |},
        ),
        unsafe("gridTemplateColumns", "[begin] 1fr 7fr 5fr 1fr [end]"),
        unsafe(
          "gridTemplateRows",
          "[begin] auto min-content [end] min-content auto",
        ),
      ]),
      sm([
        unsafe("gridTemplateColumns", "[begin] 1fr 6fr 1fr [end]"),
        gridGap(px(Theme.space(5))),
      ]),
      xs([
        unsafe("gridTemplateColumns", "[begin] 0px 1fr 0px [end]"),
        unsafe(
          "gridTemplateAreas",
          {|
           ". . ."
           ". title ."
           ". sub ."
           ". button ."
           ". . ."
           |},
        ),
        unsafe(
          "gridTemplateRows",
          "[begin] auto min-content [end] min-content min-content auto",
        ),
        gridGap(px(Theme.space(2))),
      ]),
      width(`percent(100.0)),
      height(`vh(100.0)),
      alignItems(`flexEnd),
    ]);
  let logo =
    style([
      backgroundImage(url(Icons.asDataUrl(Icons.logoBig))),
      backgroundRepeat(noRepeat),
      alignSelf(`stretch),
      unsafe("backgroundSize", "auto 100%"),
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "begin / end"),
    ]);

  let area = area => style([unsafe("gridArea", area), minHeight(px(0))]);

  let title =
    style([
      lineHeight(0.92),
      md([fontSize(px(124))]),
      sm([fontSize(px(72))]),
      xs([fontSize(px(68))]),
    ]);
};

let make = (~onSignIn, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <div className=Styles.grid>
        <div className=Styles.logo />
        <Typography
          className=(Styles.area("title") ++ " " ++ Styles.title)
          variant=`Display4>
          ("Distribute Funds" |> text)
          <br />
          ("with misthos." |> text)
        </Typography>
        <Typography className=(Styles.area("sub")) variant=`Display1>
          (
            "Misthos is the only multi-sig Bitcoin wallet that lets you change co-singers in a fast and friction-less way."
            |> text
          )
          <br />
          <br />
          ("Use it for projects. Use it for payments." |> text)
        </Typography>
        <MButton
          className=(Styles.area("button")) color=`Inherit onClick=onSignIn>
          <SvgIcon className=Css.(style([marginRight(px(16))]))>
            Icons.blockStack
          </SvgIcon>
          ("Sign In with Blockstack" |> text)
        </MButton>
      </div>
    ),
};
