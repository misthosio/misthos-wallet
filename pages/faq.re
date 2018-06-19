include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("faq");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let paragraph = data =>
  <MTypography variant=`Body1> (data |> text) </MTypography>;

let environment = Environment.get();

module T = FaqText;

let make = _children => {
  ...component,
  render: _ =>
    <Layout
      header={<Header hrefLogo=(environment.webDomain ++ "/") />}
      mobileEnabled=true>
      <Grid
        title1=("frequently asked questions" |> text)
        area3=MaterialUi.(
                <ExpansionPanel>
                  <ExpansionPanelSummary expandIcon=Icons.chevronDown>
                    <MTypography variant=`Subheading>
                      (T.whatIsMisthosQ |> text)
                    </MTypography>
                  </ExpansionPanelSummary>
                  <ExpansionPanelDetails>
                    (T.whatIsMisthosAP1 |> paragraph)
                    (T.whatIsMisthosAP2 |> paragraph)
                  </ExpansionPanelDetails>
                </ExpansionPanel>
              )
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
