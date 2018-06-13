include ViewCommon;

let component = ReasonReact.statelessComponent("Footer");

module Styles = {
  open Css;
  open BreakPoints;

  let grid =
    style([
      display(grid),
      unsafe(
        "gridTemplateAreas",
        {|
           ". . . . ."
           ". footer1 footer2 footer3 ."
           ". notice . . ."
           ". . . . ."
           |},
      ),
      unsafe("gridTemplateColumns", "[begin] 0px 1fr 1fr 1fr 0px [end]"),
      unsafe(
        "gridTemplateRows",
        "[begin] 0px min-content min-content 0px [end]",
      ),
      gridGap(px(Theme.space(2))),
    ]);

  let area = area => style([unsafe("gridArea", area)]);

  let bg =
    style([
      backgroundColor(Colors.darkGray),
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "begin / end"),
    ]);

  let logo = style([marginTop(px(Theme.space(4)))]);

  let notice =
    style([
      unsafe("gridArea", "notice"),
      fontFamily(Theme.sourceSansPro),
      fontSize(px(14)),
      color(Colors.white),
    ]);

  let link =
    style([
      display(block),
      height(px(Theme.space(3))),
      marginBottom(px(12)),
      fontFamily(Theme.oswald),
      fontWeight(600),
      fontSize(px(14)),
      color(Colors.white),
      textDecoration(underline),
      textTransform(uppercase),
    ]);
};

let make = _children => {
  ...component,
  render: _self =>
    <div className=Styles.grid>
      <div className=Styles.bg />
      <div className=(Styles.area("footer1"))>
        <div className=Styles.logo> Icons.misthosWordMark </div>
      </div>
      <div className=(Styles.area("footer2"))>
        <MTypography variant=`Headline> ("Company" |> text) </MTypography>
        <a className=Styles.link> ("Frequently Asked Questions" |> text) </a>
        <a className=Styles.link href="mailto:contact@misthos.io">
          ("Contact us" |> text)
        </a>
        <a className=Styles.link href="mailto:jobs@misthos.io">
          ("Jobs" |> text)
        </a>
        <a className=Styles.link> ({js|Datenshutzerklärung|js} |> text) </a>
        <a className=Styles.link> ("Impressum" |> text) </a>
      </div>
      <div className=(Styles.area("footer3"))>

          <MTypography variant=`Headline>
            ("Stay Connected" |> text)
          </MTypography>
        </div>
        /* form should go here */
      <div className=Styles.notice>
        ({js|© Misthos 2018. All rights reserved.|js} |> text)
      </div>
    </div>,
};
