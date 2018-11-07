include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("faq");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let paragraph = data =>
  <MTypography gutterBottom=true variant=`Body1> (data |> text) </MTypography>;

let wrapWithDiv = data =>
  <MTypography component=(`String("div")) variant=`Body1> data </MTypography>;

module Details = MaterialUi.ExpansionPanelDetails;
let environment = Environment.get();

module T = FaqText;

let make = _children => {
  ...component,
  render: _ =>
    <Layout header={<Header hrefLogo=(environment.webDomain ++ "/") />}>
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
                T.faq
                |> Array.map((item: T.t) =>
                     MaterialUi.(
                       <ExpansionPanel>
                         <ExpansionPanelSummary expandIcon=Icons.chevronDown>
                           <MTypography variant=`Subheading>
                             (item.q |> text)
                           </MTypography>
                         </ExpansionPanelSummary>
                         <Details
                           className=Css.(style([flexDirection(column)]))>
                           (
                             item.a
                             |> Array.map((line: T.line) =>
                                  switch (line) {
                                  | S(data) => data |> paragraph
                                  | E(data) => data |> wrapWithDiv
                                  }
                                )
                             |> ReasonReact.array
                           )
                         </Details>
                       </ExpansionPanel>
                     )
                   )
                |> ReasonReact.array
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
