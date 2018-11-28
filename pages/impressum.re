include ViewCommon;

let component = ReasonReact.statelessComponent("impressum");

let line = data => <MTypography variant=`Body1> {data |> text} </MTypography>;

let environment = Environment.get();

let make = _children => {
  ...component,
  render: _ =>
    <PageLayout>
      <Layout header={<Header hrefLogo={environment.webDomain ++ "/"} />}>
        <Grid
          title1={"impressum" |> text}
          area3={
            <div>
              {"Misthos UG" |> line}
              {"Friedrichstrasse 171" |> line}
              {"D10117 Berlin" |> line}
              {"Deutschland" |> line}
              <MTypography variant=`Body1>
                {"Email: " |> text}
                <a href="mailto:Contact@misthos.io">
                  {"contact@misthos.io" |> text}
                </a>
              </MTypography>
              <MTypography variant=`Body1>
                {"Link zum Impressum: " |> text}
                <a
                  href={environment.webDomain ++ "/impressum"} target="_blank">
                  {"https://www.misthos.io/impressum" |> text}
                </a>
              </MTypography>
              <MTypography variant=`Body1>
                {{js|Link zur Datenschutzerklärung: |js} |> text}
                <a
                  href={environment.webDomain ++ "/datenschutzerklarung"}
                  target="_blank">
                  {"https://www.misthos.io/datenschutzerklarung" |> text}
                </a>
              </MTypography>
            </div>
          }
        />
        <Footer />
      </Layout>
    </PageLayout>,
};
let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make([||]));
