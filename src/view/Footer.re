include ViewCommon;

let component = ReasonReact.statelessComponent("Footer");

module Styles = {
  open Css;
  open BreakPoints;

  let grid =
    style([
      display(grid),
      md([
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
      ]),
      sm([
        unsafe(
          "gridTemplateAreas",
          {|
           ". . . ."
           ". footer1 . ."
           ". footer2 footer3 ."
           ". notice notice ."
           ". . . ."
           |},
        ),
        unsafe(
          "gridTemplateRows",
          "[begin] 0px min-content min-content min-content 0px [end]",
        ),
        unsafe("gridTemplateColumns", "[begin] 0px 1fr 1fr 0px [end]"),
        gridGap(px(Theme.space(3))),
      ]),
      xs([
        unsafe(
          "gridTemplateAreas",
          {|
           ". . ."
           ". footer1 . "
           ". footer2 ."
           ". footer3 ."
           ". notice ."
           ". . ."
           |},
        ),
        unsafe(
          "gridTemplateRows",
          "[begin] 0px min-content min-content min-content min-content 0px [end]",
        ),
        unsafe("gridTemplateColumns", "[begin] 0px 1fr 0px [end]"),
        gridGap(px(Theme.space(2))),
      ]),
    ]);

  let area = area => style([unsafe("gridArea", area)]);

  let bg =
    style([
      backgroundColor(Colors.darkGray),
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "begin / end"),
    ]);

  let logo = style([marginTop(px(Theme.space(4))), display(block)]);

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

  let social = style([marginTop(px(Theme.space(4)))]);
  let socialIcon = style([marginRight(px(Theme.space(2)))]);
};

let make = _children => {
  ...component,
  render: _self => {
    let environment = Environment.get();
    <MaterialUi.MuiThemeProvider
      theme=(`ObjectGeneric(Theme.theme(~dark=true, ()) |> Theme.toJsUnsafe))>
      <div className=Styles.grid>
        <div className=Styles.bg />
        <div className=(Styles.area("footer1"))>
          <a className=Styles.logo href=(environment.webDomain ++ "/")>
            Icons.misthosWordMark
          </a>
        </div>
        <div className=(Styles.area("footer2"))>
          <MTypography
            gutterTop=true
            gutterBottom=true
            color=`TextSecondary
            variant=`Title>
            ("Company" |> text)
          </MTypography>
          <a className=Styles.link href=(environment.webDomain ++ "/faq")>
            ("Frequently Asked Questions" |> text)
          </a>
          <a className=Styles.link href="mailto:contact@misthos.io">
            ("Contact us" |> text)
          </a>
          <a className=Styles.link href="mailto:jobs@misthos.io">
            ("Jobs" |> text)
          </a>
          <a
            className=Styles.link
            href=(environment.webDomain ++ "/datenschutzerklarung")>
            ({js|Datenschutzerklärung|js} |> text)
          </a>
          <a
            className=Styles.link href=(environment.webDomain ++ "/impressum")>
            ("Impressum" |> text)
          </a>
        </div>
        <div className=(Styles.area("footer3"))>
          <MTypography
            gutterTop=true
            gutterBottom=true
            color=`TextSecondary
            variant=`Title>
            ("Stay Connected" |> text)
          </MTypography>
          MaterialUi.(
            <form
              action="https://misthos.us17.list-manage.com/subscribe/post?u=1696fffacc1f8609ca14818f3&id=e0d336cc53"
              method="post"
              target="_blank">
              <Input type_="email" placeholder="Email Address" name="EMAIL" />
              <Button type_="submit"> ("Sign Up" |> text) </Button>
              <div className=Styles.social>
                <a
                  className=Styles.socialIcon
                  href="https://twitter.com/misthosio">
                  Icons.twitter
                </a>
                <a
                  className=Styles.socialIcon
                  href="https://www.linkedin.com/company/misthos-io">
                  Icons.linkedin
                </a>
                <a
                  className=Styles.socialIcon
                  href="https://medium.com/@misthosio">
                  Icons.medium
                </a>
              </div>
            </form>
          )
        </div>
        <div className=Styles.notice>
          ({js|© Misthos 2018. All rights reserved.|js} |> text)
        </div>
      </div>
    </MaterialUi.MuiThemeProvider>;
  },
};
