include ViewCommon;

type resource =
  | Payout
  | Partner;

let component = ReasonReact.statelessComponent("NotFound");

let make = (~resource, _children) => {
  ...component,
  render: (_) =>
    <div>
      (
        text(
          "The "
          ++ (
            switch (resource) {
            | Payout => "payout"
            | Partner => "partner"
            }
          )
          ++ " was not found. Perhaps it hasn't been synced yet",
        )
      )
    </div>,
};
