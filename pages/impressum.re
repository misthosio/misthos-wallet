include ViewCommon;
[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("index");

let line = data => <MTypography variant=`Body1> (data |> text) </MTypography>;

let make = _children => {
  ...component,
  render: _ =>
    <Layout drawer=None modal=None mobileEnabled=true>
      <Grid
        title1=("impressum" |> text)
        area3=(
          [|
            line("Justin Carter"),
            line("Misthos"),
            line("Dolziger Str. 15"),
            line("D10247 Berlin"),
            line("Deutschland"),
            line("Email: contact@misthos.io"),
            line("Link zum Impressum: https://www.misthos.io/impressum"),
            line(
              {js|Link zur Datenschutzerklärung: https://www.misthos.io/datenschutzerklarung|js},
            ),
          |]
          |> ReasonReact.array
        )
      />
      <Footer />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
