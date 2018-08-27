include ViewCommon;
open Belt;

type status =
  | None
  | CreatingVenture(string);

type state = {
  newVenture: string,
  cmdStatus: CommandExecutor.cmdStatus,
  accountSettings: AccountSettings.t,
  policies: Policy.initialPolicies,
};

let policyTypeToString =
  fun
  | Policy.AtLeast(_) => "At least"
  | Policy.Unanimous => "Unanimous"
  | Policy.UnanimousMinusOne => "Unanimous minus 1"
  | Policy.Percentage(_) => "Percentage";
let stringToPolicy =
  fun
  | "At least" => Policy.atLeast(1)
  | "Unanimous" => Policy.unanimous
  | "Unanimous minus 1" => Policy.unanimousMinusOne
  | "Percentage" => Policy.percentage(50)
  | _ => Policy.unanimous;

let policyOptions =
  Policy.([|atLeast(1), percentage(50), unanimousMinusOne, unanimous|]);

let updatePolicyWithN = n =>
  fun
  | Policy.Percentage(_) => Policy.percentage(n < 0 ? 0 : n > 100 ? 100 : n)
  | Policy.AtLeast(_) => Policy.atLeast(n < 0 ? 1 : n)
  | policy => policy;

type action =
  | ChangeNewVenture(string)
  | CreateVenture
  | ChangeNumberOfCoSinger((int, int))
  | ChangeSequence(int)
  | ToggleSequence
  | ChangeAddPartnerPolicy(Policy.t)
  | ChangeAddPartnerN(string)
  | ChangeRemovePartnerPolicy(Policy.t)
  | ChangePayoutPolicy(Policy.t);

let component = ReasonReact.reducerComponent("VentureCreate");

module Styles = {
  open Css;
  let expansionPanelSummary =
    style([
      paddingLeft(px(0)),
      position(sticky),
      top(px(0)),
      backgroundColor(Colors.white),
      zIndex(100),
    ]);
  let expansionPanelDetails =
    style([flexDirection(column), paddingLeft(px(0))]);
};

let make =
    (~onCreateVenture, ~cmdStatus: CommandExecutor.cmdStatus, _children) => {
  ...component,
  initialState: () => {
    newVenture: "",
    accountSettings: AccountSettings.default,
    policies: Policy.defaultInitialPolicies,
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
    | (ChangeSequence(sequence), _) =>
      ReasonReact.Update({
        ...state,
        accountSettings: {
          ...state.accountSettings,
          sequence: Some(sequence > 65500 ? 65500 : sequence),
        },
      })
    | (ToggleSequence, _) =>
      ReasonReact.Update({
        ...state,
        accountSettings: {
          ...state.accountSettings,
          sequence:
            state.accountSettings.sequence == None ?
              Some(AccountSettings.defaultSequence) : None,
        },
      })
    | (ChangeAddPartnerPolicy(policy), _) =>
      ReasonReact.Update({
        ...state,
        policies: {
          ...state.policies,
          addPartner: policy,
          addCustodian: policy,
        },
      })
    | (ChangeAddPartnerN(n), _) =>
      let policy =
        try (
          state.policies.addPartner |> updatePolicyWithN(int_of_string(n))
        ) {
        | _ => state.policies.addPartner
        };
      ReasonReact.Update({
        ...state,
        policies: {
          ...state.policies,
          addPartner: policy,
          addCustodian: policy,
        },
      });
    | (ChangeRemovePartnerPolicy(policy), _) =>
      ReasonReact.Update({
        ...state,
        policies: {
          ...state.policies,
          removePartner: policy,
          removeCustodian: policy,
        },
      })
    | (ChangePayoutPolicy(policy), _) =>
      ReasonReact.Update({
        ...state,
        policies: {
          ...state.policies,
          payout: policy,
        },
      })

    | (CreateVenture, _) =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        onCreateVenture(name, state.accountSettings, state.policies);
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

    let policyMenuItems =
      MaterialUi.(
        policyOptions
        |. Array.mapU((. p) =>
             <MenuItem value=(`String(p |> policyTypeToString))>
               (p |> policyTypeToString |> text)
             </MenuItem>
           )
      );

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
      |. Array.slice(~offset=2, ~len=9)
      |> ReasonReact.array;
    let degradingMultiSig = state.accountSettings.sequence != None;
    let sequence = state.accountSettings.sequence;
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
                <br />
                <ScrollList>
                  <ExpansionPanel>
                    <ExpansionPanelSummary
                      className=Styles.expansionPanelSummary
                      expandIcon=Icons.chevronDown>
                      <MTypography variant=`Body2>
                        ("Additional Settings" |> text)
                      </MTypography>
                    </ExpansionPanelSummary>
                    <ExpansionPanelDetails
                      className=Styles.expansionPanelDetails>
                      <MTypography gutterBottom=true variant=`Title>
                        ("Endorsement Policies" |> text)
                      </MTypography>
                      <MTypography gutterBottom=true variant=`Body1>
                        (
                          {js|Decide how many Partners need to endorse a Proposal for it to become Accepted:|js}
                          |> text
                        )
                      </MTypography>
                      <MTypography variant=`Body2>
                        ("Partner addition:" |> text)
                      </MTypography>
                      <Select
                        value=(
                                `String(
                                  state.policies.addPartner
                                  |> policyTypeToString,
                                )
                              )
                        onChange=(
                          (e, _) =>
                            send(
                              ChangeAddPartnerPolicy(
                                extractString(e) |> stringToPolicy,
                              ),
                            )
                        )>
                        (policyMenuItems |> ReasonReact.array)
                      </Select>
                      (
                        switch (state.policies.addPartner) {
                        | Percentage({percentage}) =>
                          <FormControl>
                            <InputLabel> "N =" </InputLabel>
                            <Input
                              value=(`Int(percentage))
                              onChange=(
                                e =>
                                  send(ChangeAddPartnerN(extractString(e)))
                              )
                            />
                          </FormControl>
                        | AtLeast({n}) =>
                          <FormControl>
                            <InputLabel> "N =" </InputLabel>
                            <Input
                              value=(`Int(n))
                              /* onChange=( */
                              /*   e => */
                              /*     send( */
                              /*       ChangeSequence( */
                              /*         extractString(e) |> int_of_string, */
                              /*       ), */
                              /*     ) */
                              /* ) */
                            />
                          </FormControl>
                        | _ => ReasonReact.null
                        }
                      )
                      <MTypography variant=`Body2>
                        ("Partner removal:" |> text)
                      </MTypography>
                      <Select
                        value=(
                                `String(
                                  state.policies.removePartner
                                  |> policyTypeToString,
                                )
                              )
                        onChange=(
                          (e, _) =>
                            send(
                              ChangeRemovePartnerPolicy(
                                extractString(e) |> stringToPolicy,
                              ),
                            )
                        )>
                        (policyMenuItems |> ReasonReact.array)
                      </Select>
                      <MTypography variant=`Body2>
                        ("Payout:" |> text)
                      </MTypography>
                      <Select
                        value=(
                                `String(
                                  state.policies.payout |> policyTypeToString,
                                )
                              )
                        onChange=(
                          (e, _) =>
                            send(
                              ChangePayoutPolicy(
                                extractString(e) |> stringToPolicy,
                              ),
                            )
                        )>
                        (policyMenuItems |> ReasonReact.array)
                      </Select>
                      <MTypography gutterBottom=true variant=`Title>
                        ("Wallet Settings" |> text)
                      </MTypography>
                      <MTypography gutterBottom=true variant=`Body1>
                        (
                          {js|You may adjust the wallet settings for your Venture
                       here. Once the Venture is created, these settings may not
                       be changed, so please choose wisely.|js}
                          |> text
                        )
                      </MTypography>
                      <MTypography
                        gutterTop=true gutterBottom=true variant=`Subheading>
                        ("Degrading Multisig" |> text)
                      </MTypography>
                      <MTypography
                        gutterTop=true gutterBottom=true variant=`Body1>
                        (
                          {js|The degrading multisig feature adds a time-release
                           to funds that have been locked due to custodian
                           changes.|js}
                          |> text
                        )
                      </MTypography>
                      <Grid container=true direction=`Row alignItems=`Baseline>
                        <Grid item=true xs=V8>
                          <FormControlLabel
                            control={
                              <Switch
                                color=`Primary
                                checked=(`Bool(degradingMultiSig))
                                onChange=((_, _) => send(ToggleSequence))
                              />
                            }
                            label=("Degrading Multisig" |> text)
                          />
                        </Grid>
                        <Grid item=true xs=V4>
                          {
                            let value =
                              switch (sequence) {
                              | Some(s) => `Int(s)
                              | None => `String("")
                              };
                            <FormControl
                              disabled=(! degradingMultiSig) fullWidth=true>
                              <InputLabel> "Unlock after" </InputLabel>
                              <Input
                                value
                                onChange=(
                                  e =>
                                    send(
                                      ChangeSequence(
                                        extractString(e) |> int_of_string,
                                      ),
                                    )
                                )
                                endAdornment={
                                  <InputAdornment position=`End>
                                    ("blocks" |> text)
                                  </InputAdornment>
                                }
                              />
                              <FormHelperText>
                                (
                                  (
                                    switch (sequence) {
                                    | Some(s) =>
                                      "Approx. "
                                      ++ (s / (6 * 24) |> string_of_int)
                                      ++ " Days"
                                    | None => "Disabled"
                                    }
                                  )
                                  |> text
                                )
                              </FormHelperText>
                            </FormControl>;
                          }
                        </Grid>
                      </Grid>
                      <MTypography
                        gutterTop=true gutterBottom=true variant=`Subheading>
                        ("Required Signatures" |> text)
                      </MTypography>
                      <MTypography
                        gutterTop=true gutterBottom=true variant=`Body1>
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
                    </ExpansionPanelDetails>
                  </ExpansionPanel>
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
