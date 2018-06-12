[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

/* module Head = { */
/*   [@bs.module "next/head"] external head : ReasonReact.reactClass = "default"; */

/*   let make = children => */
/*     ReasonReact.wrapJsForReason(~reactClass=head, ~props=None, children); */
/* }; */
/* module WithRoot = { */
/*   let component = ReasonReact.statelessComponent("WithRoot"); */
/*   let theme = Theme.theme |> Theme.toJsUnsafe; */
/*   let make = children => { */
/*     ...component, */
/*     render: self_ => { */
/*       Js.log2("render", self_); */
/*       MaterialUi.( */
/*         <MuiThemeProvider theme=(`ObjectGeneric(theme))> */
/*           children */
/*         </MuiThemeProvider> */
/*       ); */
/*     }, */
/*   }; */
/* }; */
let component = ReasonReact.statelessComponent("index");
let make = _children => {
  ...component,
  render: _ =>
    <Layout drawer=None modal=None>
      <PublicHome onSignIn=(_e => Session.signIn() |> ignore) />
    </Layout>,
};
let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
