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
          <ContactUsShoutOut />
          <CommandExecutor.Status action=CreateVenture cmdStatus />
        </div>
      }
      area4={<VentureInfoBox />}
    />,
};
