let component = ReasonReact.statelessComponent("PublicHome");

let make = (~updateSession, _children) => {
  ...component,
  render: _self => {
    let onClick = _e => updateSession(UserSession.SignIn);
    MaterialUi.(
      <Button color=`Inherit onClick> "Sign In with Blockstack" </Button>
    );
  },
};
