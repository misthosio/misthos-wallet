include ViewCommon;
open Belt;

type status =
  | None
  | CreatingVenture(string);

type state = {
  newVenture: string,
  cmdStatus: CommandExecutor.cmdStatus,
  accountSettings: AccountSettings.t,
};

type action =
  | ChangeNewVenture(string)
  | CreateVenture
  | ChangeNumberOfCoSinger((int, int))
  | ChangeSequence(int);

let component = ReasonReact.reducerComponent("VentureCreate");

let make =
    (~onCreateVenture, ~cmdStatus: CommandExecutor.cmdStatus, _children) => {
  ...component,
  initialState: () => {
    newVenture: "",
    accountSettings: AccountSettings.default,
    cmdStatus,
  },
  willReceiveProps: ({state}) => {...state, cmdStatus},
  reducer: (action, state) =>
    switch (action, state.cmdStatus) {
    | (_, Pending(_)) => ReasonReact.NoUpdate
    | (ChangeNewVenture(text), _) =>
      ReasonReact.Update({...state, newVenture: text})
    | (ChangeNumberOfCoSinger((idx, nCoSigners)), _) =>
      ReasonReact.Update({
        ...state,
        accountSettings: {
          ...state.accountSettings,
          coSignerList:
            state.accountSettings.coSignerList
            |. Array.mapWithIndex((i, x) => idx == i ? nCoSigners : x),
        },
      })
    | (CreateVenture, _) =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        onCreateVenture(
          name,
          state.accountSettings,
          Policy.defaultInitialPolicies,
        );
        ReasonReact.NoUpdate;
      }
    },
  render: ({send, state}) => {
    let onSubmit = ignoreEvent(() => send(CreateVenture));
    let onClick = () => send(CreateVenture);
    let stringOfMultiSig = (nCoSigners, requiredSigners) =>
      string_of_int(requiredSigners)
      ++ "-of-"
      ++ string_of_int(nCoSigners)
      |> text;

    let getMenuItems = nCoSigners =>
      Array.range(1, nCoSigners)
      |. Array.mapU((. idx) =>
           MaterialUi.(
             <MenuItem value=(`Int(idx))>
               (stringOfMultiSig(nCoSigners, idx))
             </MenuItem>
           )
         );
    let nSigs =
      state.accountSettings.coSignerList
      |. Array.mapWithIndexU((. idx, nCoSigners) =>
           MaterialUi.(
             <TableRow>
               <TableCell>
                 <MTypography variant=`Body2>
                   (string_of_int(idx) |> text)
                 </MTypography>
               </TableCell>
               <TableCell numeric=true>
                 <Select
                   value=(`Int(nCoSigners))
                   onChange=(
                     (e, _) =>
                       send(ChangeNumberOfCoSinger((idx, extractString(e))))
                   )>
                   (getMenuItems(idx) |> ReasonReact.array)
                 </Select>
               </TableCell>
             </TableRow>
           )
         )
      |. Array.slice(~offset=1, ~len=10)
      |> ReasonReact.array;
    <Grid
      title1=("Create a Venture" |> text)
      area3=MaterialUi.(
              <form onSubmit className=ScrollList.containerStyles>
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
                <ScrollList>
                  <MTypography
                    gutterTop=true gutterBottom=true variant=`Subheading>
                    ("Required Signatures" |> text)
                  </MTypography>
                  <MTypography gutterTop=true gutterBottom=true variant=`Body1>
                    (
                      {js|Select the number of signatures your Venture will
                       require for transactions, depending on the number of
                       Partners:|js}
                      |> text
                    )
                  </MTypography>
                  <Table>
                    <TableHead>
                      <TableRow>
                        <TableCell>
                          <MTypography variant=`Body2>
                            ("NUMBER OF PARTNERS" |> text)
                          </MTypography>
                        </TableCell>
                        <TableCell numeric=true>
                          <MTypography variant=`Body2>
                            ("REQUIRED SIGNATURES" |> text)
                          </MTypography>
                        </TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody> nSigs </TableBody>
                  </Table>
                </ScrollList>
                <SingleActionButton
                  onSubmit=onClick
                  canSubmitAction=true
                  withConfirmation=false
                  action=CommandExecutor.Status.CreateVenture
                  buttonText="create venture"
                  cmdStatus
                />
                <ContactUsShoutOut />
              </form>
            )
      area4={<VentureInfoBox />}
    />;
  },
};
