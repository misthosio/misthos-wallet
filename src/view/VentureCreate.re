include ViewCommon;

type status =
  | None
  | CreatingVenture(string);

type state = {
  newVenture: string,
  cmdStatus: CommandExecutor.cmdStatus,
};

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
    ]);
};

let make =
    (~onCreateVenture, ~cmdStatus: CommandExecutor.cmdStatus, _children) => {
  ...component,
  initialState: () => {newVenture: "", cmdStatus},
  willReceiveProps: ({state}) => {...state, cmdStatus},
  reducer: (action, state) =>
    switch (action, state.cmdStatus) {
    | (_, Pending(_)) => ReasonReact.NoUpdate
    | (ChangeNewVenture(text), _) =>
      ReasonReact.Update({...state, newVenture: text})
    | (CreateVenture, _) =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        onCreateVenture(name);
        ReasonReact.NoUpdate;
      }
    },
  render: ({send, state}) =>
    <Grid
      title1=("Create a Venture" |> text)
      area3={
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
          <MTypography gutterTop=true variant=`Title>
            ("Venture Name" |> text)
          </MTypography>
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
          <CommandExecutor.Status action=CreateVenture cmdStatus />
        </div>
      }
      area4={
        <div className=Styles.infoBox>
          <MTypography gutterTop=true gutterBottom=true variant=`Title>
            ("What can you do with a venture?" |> text)
          </MTypography>
          <MTypography gutterBottom=true variant=`Body2>
            (
              {js|
                 • Your Venture can receive money from different sources, such as customers, clients, and investors
                |js}
              |> text
            )
          </MTypography>
          <MTypography gutterBottom=true variant=`Body2>
            (
              {js|
                 • Every Partner of the Venture has full transparency of income and payouts
                |js}
              |> text
            )
          </MTypography>
          <MTypography gutterBottom=true variant=`Body2>
            (
              {js|
                 • The team decides the Policies by which payouts take place
                |js}
              |> text
            )
          </MTypography>
        </div>
      }
    />,
};
