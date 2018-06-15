include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("frequentlyAskedQuestions");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let make = _children => {
  ...component,
  render: _ =>
    <Layout drawer=None modal=None mobileEnabled=true>
      <Grid
        title1=("frequently asked questions" |> text)
        area3=([|line("TODO")|] |> ReasonReact.array)
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
