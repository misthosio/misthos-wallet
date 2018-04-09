let component = ReasonReact.statelessComponent("PublicHome");

let make = (~onSignIn, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Button color=`Inherit onClick=onSignIn>
        "Sign In with Blockstack"
      </Button>
    ),
};
