include ViewCommon;

type status =
  | None
  | CreatingVenture(string);

type state = {newVenture: string};

type action =
  | ChangeNewVenture(string)
  | CreateVenture;

let component = ReasonReact.reducerComponent("VentureCreate");

module Styles = {
  open Css;
  let infoBox =
    style([
      border(px(2), solid, Colors.black),
      padding4(
        ~top=px(0),
        ~right=px(Theme.space(4)),
        ~left=px(Theme.space(4)),
        ~bottom=px(Theme.space(4)),
      ),
      marginTop(px(32)),
    ]);
};

let make = (~onCreateVenture, ~cmdStatus: CommandStatus.t, _children) => {
  ...component,
  initialState: () => {newVenture: ""},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewVenture(text) => ReasonReact.Update({newVenture: text})
    | CreateVenture =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        onCreateVenture(name);
        ReasonReact.Update({newVenture: ""});
      }
    },
  render: ({send, state}) => {
    let feedback =
      switch (cmdStatus) {
      | Pending(_) => <Spinner text="waiting for result" />
      | Error(_) => "Could not execute teh command" |> text
      | _ => ReasonReact.null
      };
    <Body2
      titles=["Create a Venture"]
      body1=
        <div>
          <MTypography variant=`Body2>
            (
              {js|
                 Set up a new Venture with yourself as the initial Partner.
                 You can add and remove Partners once the Venture is created.
                 But first, let’s start with a name.
                |js}
              |> text
            )
          </MTypography>
          <MTypography variant=`Title> ("Venture Name" |> text) </MTypography>
          <MInput
            placeholder="Enter a Venture Name"
            value=(`String(state.newVenture))
            onChange=(e => send(ChangeNewVenture(extractString(e))))
            autoFocus=true
            fullWidth=true
          />
          <MButton fullWidth=true onClick=(_e => send(CreateVenture))>
            ("create venture" |> text)
          </MButton>
          feedback
        </div>
      body2=
        <div className=Styles.infoBox>
          <MTypography variant=`Title>
            ("What can you do with a venture?" |> text)
          </MTypography>
          <MTypography variant=`Body2>
            (
              {js|
                 • Your Venture can receive money from different sources, such as customers, clients, and investors
                |js}
              |> text
            )
          </MTypography>
          <MTypography variant=`Body2>
            (
              {js|
                 • Every Partner of the Venture has full transparency of income and payouts
                |js}
              |> text
            )
          </MTypography>
          <MTypography variant=`Body2>
            (
              {js|
                 • The team decides the Policies by which payouts take place
                |js}
              |> text
            )
          </MTypography>
        </div>
    />;
  },
};
