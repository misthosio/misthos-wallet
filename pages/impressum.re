include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("impressum");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let environment = Environment.get();

let make = _children => {
  ...component,
  render: _ =>
    <Layout drawer=None modal=None mobileEnabled=true>
      <Grid
        title1=("impressum" |> text)
        area3={
          <div>
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
            <MTypography variant=`Body1>
              ({js|Link zur Datenschutzerklärung: |js} |> text)
              <a
                href=(environment.webDomain ++ "/datenschutzerklarung")
                target="_blank">
                ("https://www.misthos.io/datenschutzerklarung" |> text)
              </a>
            </MTypography>
          </div>
        }
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
