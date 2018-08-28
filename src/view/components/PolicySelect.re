open Belt;

include ViewCommon;

type state = {
  selectedPolicy: Policy.t,
  inputNumber: string,
};

type action =
  | SelectPolicyType(Policy.t)
  | SelectPolicyNumber(string);

let component = ReasonReact.reducerComponent("PolicySelect");

module Styles = {
  open Css;
  let container = style([marginBottom(px(Theme.space(2)))]);
  let flex = style([flex(1)]);
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
  | "Percentage" => Policy.percentage(51)
  | _ => Policy.unanimous;

let policyOptions =
  Policy.([|atLeast(1), percentage(51), unanimousMinusOne, unanimous|]);

let updatePolicyWithN = n =>
  fun
  | Policy.Percentage(_) => {
      let n = n < 0 ? 0 : n > 100 ? 100 : n;
      (Policy.percentage(n), n |> string_of_int);
    }
  | Policy.AtLeast(_) => {
      let n = n < 0 ? 1 : n;
      (Policy.atLeast(n), n |> string_of_int);
    }
  | policy => (policy, "");

let extractPolicyNumber =
  fun
  | Policy.Percentage({percentage}) => string_of_int(percentage)
  | Policy.AtLeast({n}) => string_of_int(n)
  | _ => "";

type policySelection =
  | ValidSelection(Policy.t)
  | InvalidSelection;

let make =
    (
      ~label: string,
      ~initialValue: Policy.t,
      ~onChange: policySelection => unit,
      _children,
    ) => {
  ...component,
  initialState: () => {
    selectedPolicy: initialValue,
    inputNumber: extractPolicyNumber(initialValue),
  },
  reducer: (action, state) =>
    switch (action) {
    | SelectPolicyType(policy) =>
      let state = {
        selectedPolicy: policy,
        inputNumber: extractPolicyNumber(policy),
      };
      onChange(ValidSelection(state.selectedPolicy));
      ReasonReact.Update(state);
    | SelectPolicyNumber(n) =>
      switch (n) {
      | "" =>
        onChange(InvalidSelection);
        ReasonReact.Update({...state, inputNumber: ""});
      | n =>
        try (
          {
            let (selectedPolicy, inputNumber) =
              state.selectedPolicy |> updatePolicyWithN(int_of_string(n));
            let state = {selectedPolicy, inputNumber};
            onChange(ValidSelection(state.selectedPolicy));
            ReasonReact.Update(state);
          }
        ) {
        | _ => ReasonReact.NoUpdate
        }
      }
    },
  render: ({state, send}) => {
    open MaterialUi;
    let policyMenuItems =
      policyOptions
      |. Array.mapU((. p) =>
           <MaterialUi.MenuItem value=(`String(p |> policyTypeToString))>
             (p |> policyTypeToString |> text)
           </MaterialUi.MenuItem>
         );
    <Grid container=true className=Styles.container>
      <FormControl className=Styles.flex>
        <InputLabel> (label |> text) </InputLabel>
        <Select
          value=(`String(state.selectedPolicy |> policyTypeToString))
          onChange=(
            (e, _) =>
              extractString(e) |> stringToPolicy |. SelectPolicyType |> send
          )>
          (policyMenuItems |> ReasonReact.array)
        </Select>
      </FormControl>
      (
        switch (state.selectedPolicy) {
        | Policy.Percentage(_) =>
          <FormControl error=(state.inputNumber == "")>
            <InputLabel> "% =" </InputLabel>
            <Input
              value=(`String(state.inputNumber))
              onChange=(e => extractString(e) |. SelectPolicyNumber |> send)
            />
          </FormControl>
        | AtLeast(_) =>
          <FormControl error=(state.inputNumber == "")>
            <InputLabel> "N =" </InputLabel>
            <Input
              value=(`String(state.inputNumber))
              onChange=(e => extractString(e) |. SelectPolicyNumber |> send)
            />
          </FormControl>
        | _ => ReasonReact.null
        }
      )
    </Grid>;
  },
};
