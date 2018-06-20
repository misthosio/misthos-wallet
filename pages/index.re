[@bs.module "../src/web/withRoot"] [@bs.val]
external withRoot : 'a => 'a = "default";

let component = ReasonReact.statelessComponent("index");
let make = _children => {
  ...component,
  render: _ =>
    <Layout mobileEnabled=true>
      <PublicHome onSignIn=(_e => Session.signIn() |> ignore) />
      <MisthosIs
        primary="Collaborative"
        secondary="Join a Venture or start your own. Propose and endorse decisions based on team consensus."
        img={
          <img
            height="480px"
            width="350px"
            src="/static/img/misthos-gif-01-v01.gif"
          />
        }
      />
      <MisthosIs
        primary="Intuitive"
        secondary="Focus on simplicity. Complete tasks quickly with an easy-to-use interface."
        img={
          <img
            height="480px"
            width="350px"
            src="/static/img/misthos-gif-02-v01.gif"
          />
        }
      />
      <MisthosIs
        last=true
        primary="Dynamic"
        secondary="See all Venture activity instantaneously. Manage Partners and create payouts with ease."
        img={
          <img
            height="480px"
            width="350px"
            src="/static/img/misthos-gif-03-v01.gif"
          />
        }
      />
      <Footer />
    </Layout>,
};

let default =
  withRoot(ReasonReact.wrapReasonForJs(~component, _jsProps => make([||])));
