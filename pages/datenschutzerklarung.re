include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("datenshutzerklarung");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;
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
          <div>
            <MTypography variant=`Title> (T.title |> text) </MTypography>
            (T.section1 |> line)
            (T.section2Heading |> subheading)
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
              <a href=(environment.webDomain ++ "/impressum") target="_blank">
                ("https://www.misthos.io/impressum" |> text)
              </a>
            </MTypography>
            (T.section3Heading |> subheading)
            <MTypography variant=`Body1> T.section3 </MTypography>
            (T.section4Heading |> subheading)
            (T.section4 |> line)
          </div>
        }
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
