include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("faq");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let paragraph = data =>
  <MTypography gutterBottom=true variant=`Body1> (data |> text) </MTypography>;

let panel = (heading, details) =>
  MaterialUi.(
    <ExpansionPanel>
      <ExpansionPanelSummary expandIcon=Icons.chevronDown>
        <MTypography variant=`Subheading> (heading |> text) </MTypography>
      </ExpansionPanelSummary>
      details
    </ExpansionPanel>
  );

module Details = MaterialUi.ExpansionPanelDetails;
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
        area3={
          <div
            className=(
              ScrollList.containerStyles
              ++ " "
              ++ Css.(style([paddingBottom(px(Theme.space(4)))]))
            )>
            <ScrollList>
              (
                panel(
                  T.whatIsMisthosQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.whatIsMisthosAP1 |> paragraph)
                    (T.whatIsMisthosAP2 |> paragraph)
                    (T.whatIsMisthosAP3 |> paragraph)
                  </Details>,
                )
              )
              (
                panel(
                  T.whoIsMisthosForQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.whoIsMisthosForA |> paragraph)
                  </Details>,
                )
              )
              (
                panel(
                  T.howCanATeamUseMisthosTodayQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    T.howCanATeamUseMisthosTodayA
                  </Details>,
                )
              )
              (
                panel(
                  T.whatIsUniqueAboutMisthosQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.whatIsUniqueAboutMisthosAP1 |> paragraph)
                    T.whatIsUniqueAboutMisthosAP2
                  </Details>,
                )
              )
              (
                panel(
                  T.whatAreThePrinciplesBehindMisthosQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    T.whatAreThePrinciplesBehindMisthosA
                  </Details>,
                )
              )
              (
                panel(
                  T.whatIsBlockstackQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    T.whatIsBlockstackA
                  </Details>,
                )
              )
              (
                panel(
                  T.doINeedToRegisterWithBlockstackQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.doINeedToRegisterWithBlockstackA |> paragraph)
                  </Details>,
                )
              )
              (
                panel(
                  T.howCanIGetStartedQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.howCanIGetStartedAP1 |> paragraph)
                    T.howCanIGetStartedAP2
                  </Details>,
                )
              )
              (
                panel(
                  T.whatIsAMisthosVentureQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.whatIsAMisthosVentureAP1 |> paragraph)
                    T.whatIsAMisthosVentureAP2
                  </Details>,
                )
              )
              (
                panel(
                  T.howCreateAVentureQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    T.howCreateAVentureAP1
                    (T.howCreateAVentureAP2 |> paragraph)
                    (T.howCreateAVentureAP3 |> paragraph)
                  </Details>,
                )
              )
              (
                panel(
                  T.howJoinVentureQ,
                  <Details className=Css.(style([flexDirection(column)]))>
                    (T.howJoinVentureAP1 |> paragraph)
                    (T.howJoinVentureAP2 |> paragraph)
                    T.howJoinVentureAP3
                  </Details>,
                )
              )
            </ScrollList>
          </div>
        }
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
