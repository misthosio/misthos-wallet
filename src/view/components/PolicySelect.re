open Belt;

include ViewCommon;

let component = ReasonReact.statelessComponent("PolicySelect");

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

let make = (~value: Policy.t, ~onChange: Policy.t => unit, _children) => {
  ...component,
  render: _ => {
    open MaterialUi;
    let policyMenuItems =
      MaterialUi.(
        policyOptions
        |. Array.mapU((. p) =>
             <MenuItem value=(`String(p |> policyTypeToString))>
               (p |> policyTypeToString |> text)
             </MenuItem>
           )
      );
    <Grid container=true>
      <Select
        value=(`String(value |> policyTypeToString))
        onChange=((e, _) => extractString(e) |> stringToPolicy |> onChange)>
        (policyMenuItems |> ReasonReact.array)
      </Select>
      (
        switch (value) {
        | Policy.Percentage({percentage}) =>
          <FormControl>
            <InputLabel> "N =" </InputLabel>
            <Input
              value=(`Int(percentage))
              onChange=(
                e =>
                  Policy.Percentage({
                    percentage: extractString(e) |> int_of_string,
                  })
                  |> onChange
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
    </Grid>;
  },
};
