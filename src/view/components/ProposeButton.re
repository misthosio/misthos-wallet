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

let component = ReasonReact.reducerComponent("ProcessApprovalButtons");

module Styles = {
  open Css;
  let gray = style([color(rgba(0, 0, 0, 0.38))]);
  let inlineConfirm = style([display(`flex), alignItems(`baseline)]);
};

let make =
    (
      ~proposeText,
      ~onPropose,
      ~canSubmitProposal,
      ~withConfirmation=true,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {buttonState: NoDecision, cmdStatus: Idle},
  willReceiveProps: ({state}) => {...state, cmdStatus},
  reducer: (action, state) =>
    switch (action, withConfirmation, canSubmitProposal) {
    | (_, _, false) => ReasonReact.NoUpdate
    | (Propose, true, _) =>
      ReasonReact.Update({...state, buttonState: ConfirmProposal})
    | (Propose, false, _) =>
      ReasonReact.SideEffects((({send}) => send(ConfirmProposal)))
    | (ConfirmProposal, _, _) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: ProposalSubmited},
        ((_) => onPropose()),
      )
    | (Cancel, _, _) =>
      ReasonReact.Update({cmdStatus: Idle, buttonState: NoDecision})
    },
  render: ({send, state: {buttonState: state, cmdStatus}}) =>
    ReasonReact.array(
      Array.concatMany([|
        switch (state, cmdStatus) {
        | (_, Error(_))
        | (NoDecision, _) => [|
            <MButton fullWidth=true onClick=(_e => send(Propose))>
              (text(proposeText))
            </MButton>,
            <CommandExecutor.Status cmdStatus action=Proposal />,
          |]
        | (ConfirmProposal, _) => [|
            <MTypography className=Styles.inlineConfirm variant=`Body2>
              (proposeText |> text)
              <MButton variant=Flat onClick=(_e => send(ConfirmProposal))>
                (text("yes"))
              </MButton>
              <MButton variant=Flat onClick=(_e => send(Cancel))>
                (text("No"))
              </MButton>
            </MTypography>,
          |]
        | (ProposalSubmited, _) => [|
            <CommandExecutor.Status cmdStatus action=Proposal />,
          |]
        },
      |]),
    ),
};
