open Belt;

include ViewCommon;

type buttonState =
  | NoDecision
  | ConfirmProposal
  | ProposalSubmited;

type state = {
  buttonState,
  cmdStatus: CommandExecutor.cmdStatus,
};

type action =
  | Cancel
  | Propose
  | ConfirmProposal;

let component = ReasonReact.reducerComponent("SingleActionButton");

module Styles = {
  open Css;
  let inlineConfirm = style([display(`flex), alignItems(`baseline)]);
  let warning = style([color(Colors.error)]);
};

let make =
    (
      ~buttonText,
      ~alertText=?,
      ~onSubmit,
      ~onPropose=?,
      ~onCancel=?,
      ~canSubmitAction,
      ~withConfirmation=true,
      ~action=CommandExecutor.Status.Proposal,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {
    buttonState:
      switch (cmdStatus) {
      | PreSubmit(_)
      | Pending(_) => ProposalSubmited
      | _ => NoDecision
      },
    cmdStatus,
  },
  willReceiveProps: ({state}) => {...state, cmdStatus},
  reducer: (action, state) =>
    switch (action, withConfirmation, canSubmitAction) {
    | (_, _, false) => ReasonReact.NoUpdate
    | (Propose, true, _) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: ConfirmProposal},
        (_ => onPropose |> Utils.mapOption(f => f()) |> ignore),
      )
    | (Propose, false, _) =>
      ReasonReact.SideEffects((({send}) => send(ConfirmProposal)))
    | (ConfirmProposal, _, _) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: ProposalSubmited},
        (_ => onSubmit()),
      )
    | (Cancel, _, _) =>
      ReasonReact.UpdateWithSideEffects(
        {cmdStatus: Idle, buttonState: NoDecision},
        (_ => onCancel |> Utils.mapOption(f => f()) |> ignore),
      )
    },
  render: ({send, state: {buttonState: state, cmdStatus}}) =>
    <div>
      {
        ReasonReact.array(
          Array.concatMany([|
            switch (state, cmdStatus) {
            | (ConfirmProposal, _) => [|
                <MTypography className=Styles.warning variant=`Body2>
                  {alertText |> Js.Option.getWithDefault("") |> text}
                </MTypography>,
                <MTypography className=Styles.inlineConfirm variant=`Body2>
                  {buttonText |> text}
                  <MButton
                    gutterTop=false
                    variant=Flat
                    onClick=(_e => send(ConfirmProposal))>
                    {text("yes")}
                  </MButton>
                  <MButton
                    gutterTop=false variant=Flat onClick=(_e => send(Cancel))>
                    {text("No")}
                  </MButton>
                </MTypography>,
              |]
            | (_, PreSubmitError(_) | Error(_) | Idle)
            | (NoDecision, _) => [|
                <MButton
                  fullWidth=true onClick=(_e => send(Propose)) submitBtn=true>
                  {text(buttonText)}
                </MButton>,
                <CommandExecutor.Status cmdStatus action />,
              |]
            | (ProposalSubmited, _) => [|
                <CommandExecutor.Status cmdStatus action />,
              |]
            },
          |]),
        )
      }
    </div>,
};
