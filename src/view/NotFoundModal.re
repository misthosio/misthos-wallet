include ViewCommon;

type resource =
  | Payout
  | Partner;

let component = ReasonReact.statelessComponent("NotFound");

let make = (~resource, _children) => {
  ...component,
  render: (_) => {
    let resourceText =
      switch (resource) {
      | Payout => "payout"
      | Partner => "partner"
      };
    <Body2
      titles=[resourceText ++ " not found"]
      body1=
        <div>
          (
            text(
              "The "
              ++ resourceText
              ++ " was not found. Perhaps it hasn't been synced yet",
            )
          )
        </div>
      body2=ReasonReact.null
    />;
  },
};
