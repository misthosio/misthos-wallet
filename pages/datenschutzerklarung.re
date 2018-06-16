include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("datenshutzerklarung");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;
let paragraph = data =>
  <MTypography variant=`Body1> <p> (data |> text) </p> </MTypography>;
let subheading = data =>
  <MTypography variant=`Subheading> (data |> text) </MTypography>;

module T = Text.DatenschutzErklaerung;

let environment = Environment.get();

let make = _children => {
  ...component,
  render: _ =>
    <Layout drawer=None modal=None mobileEnabled=true>
      <Grid
        title1=(T.title |> text)
        area3={
          <div
            className=(
              ScrollList.containerStyles
              ++ " "
              ++ Css.(style([paddingBottom(px(Theme.space(4)))]))
            )>
            <ScrollList>
              (T.section1 |> paragraph)
              (T.section2Heading |> subheading)
              <p>
                ("Justin Carter" |> line)
                ("Misthos" |> line)
                ("Dolziger Str. 15" |> line)
                ("D10247 Berlin" |> line)
                ("Deutschland" |> line)
                <MTypography variant=`Body1>
                  ("Email: " |> text)
                  <a href="mailto:Contact@misthos.io">
                    ("contact@misthos.io" |> text)
                  </a>
                </MTypography>
                <MTypography variant=`Body1>
                  ("Link zum Impressum: " |> text)
                  <a
                    href=(environment.webDomain ++ "/impressum")
                    target="_blank">
                    ("https://www.misthos.io/impressum" |> text)
                  </a>
                </MTypography>
              </p>
              (T.section3Heading |> subheading)
              <MTypography variant=`Body1> T.section3 </MTypography>
              (T.section4Heading |> subheading)
              (T.section4 |> paragraph)
              (T.section5Heading |> subheading)
              <MTypography variant=`Body1> T.section5 </MTypography>
              (T.section6Heading |> subheading)
              (T.section6P1 |> paragraph)
              (T.section6P2 |> paragraph)
              (T.section6P3 |> paragraph)
              (T.section6P4 |> paragraph)
              (T.section6P5 |> paragraph)
              (T.section6P6 |> paragraph)
              (T.section7Heading |> subheading)
              (T.section7 |> paragraph)
              (T.section8Heading |> subheading)
              (T.section8P1 |> paragraph)
              (T.section8P2 |> paragraph)
              (T.section9Heading |> subheading)
              (T.section9P1 |> paragraph)
              (T.section9P2 |> paragraph)
              (T.section10Heading |> subheading)
              (T.section10 |> paragraph)
              (T.section11Heading |> subheading)
              (T.section11P1 |> paragraph)
              (T.section11P2 |> paragraph)
              (T.section11P3 |> paragraph)
              (T.section11P4 |> paragraph)
              (T.section11P5 |> paragraph)
              (T.section12Heading |> subheading)
              (T.section12 |> paragraph)
              (T.section13Heading |> subheading)
              (T.section13 |> paragraph)
              (T.section14Heading |> subheading)
              (T.section14P1 |> paragraph)
              (T.section14P2 |> paragraph)
              (T.section14P3 |> paragraph)
              <MTypography variant=`Body1> T.section14P4 </MTypography>
            </ScrollList>
          </div>
        }
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
