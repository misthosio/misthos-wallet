include ViewCommon;

type resource =
  | Payout
  | Income
  | Partner;

let component = ReasonReact.statelessComponent("NotFound");

let make = (~resource, _children) => {
  ...component,
  render: _ => {
    let resourceText =
      switch (resource) {
      | Payout => "payout"
      | Partner => "partner"
      | Income => "income transaction"
      };
    <Grid
      title1=(resourceText ++ " not found" |> text)
      area3={
        <div>
          (
            text(
              "The "
              ++ resourceText
              ++ " was not found. Perhaps the data hasn't been synced yet",
            )
          )
        </div>
      }
    />;
  },
};
