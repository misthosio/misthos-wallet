[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("index");
let make = _children => {
  ...component,
  render: _ =>
    <Layout mobileEnabled=true>
      <PublicHome onSignIn=(_e => Session.signIn() |> ignore) />
    </Layout>,
};

let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
