open Belt;

include ViewCommon;

type buttonState =
  | NoDecision
  | ConfirmEndorse
  | EndorsementSubmited
  | ConfirmReject
  | RejectionSubmited;

type state = {
  buttonState,
  cmdStatus: CommandExecutor.cmdStatus,
};

type action =
  | Cancel
  | Endorse
  | ConfirmEndorse
  | Reject
  | ConfirmReject;

let component = ReasonReact.reducerComponent("ProcessApprovalButtons");

module Styles = {
  open Css;
  let gray = style([color(rgba(0, 0, 0, 0.38))]);
  let inlineConfirm = style([display(`flex), alignItems(`baseline)]);
};

let make =
    (
      ~endorseText,
      ~rejectText,
      ~canVote,
      ~onEndorse,
      ~onReject,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {buttonState: NoDecision, cmdStatus: Idle},
  willReceiveProps: ({state}) => {...state, cmdStatus},
  reducer: (action, state) =>
    switch (action) {
    | Endorse => ReasonReact.Update({...state, buttonState: ConfirmEndorse})
    | Reject => ReasonReact.Update({...state, buttonState: ConfirmReject})
    | ConfirmEndorse =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: EndorsementSubmited},
        ((_) => onEndorse()),
      )
    | ConfirmReject =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: RejectionSubmited},
        ((_) => onReject()),
      )
    | Cancel => ReasonReact.Update({cmdStatus: Idle, buttonState: NoDecision})
    },
  render: ({send, state: {buttonState: state, cmdStatus}}) =>
    ReasonReact.array(
      Array.concatMany([|
        switch (state, canVote) {
        | (NoDecision, true) => [|
            <MButton fullWidth=true onClick=(_e => send(Endorse))>
              (text(endorseText))
            </MButton>,
            <MButton
              className=Styles.gray variant=Flat onClick=(_e => send(Reject))>
              (text(rejectText))
            </MButton>,
          |]
        | (NoDecision, false) => [|ReasonReact.null|]
        | (ConfirmReject, _) => [|
            <MTypography className=Styles.inlineConfirm variant=`Body2>
              (rejectText |> text)
              <MButton variant=Flat onClick=(_e => send(ConfirmReject))>
                (text("yes"))
              </MButton>
              <MButton variant=Flat onClick=(_e => send(Cancel))>
                (text("No"))
              </MButton>
            </MTypography>,
          |]
        | (ConfirmEndorse, _) => [|
            <MTypography className=Styles.inlineConfirm variant=`Body2>
              (endorseText |> text)
              <MButton variant=Flat onClick=(_e => send(ConfirmEndorse))>
                (text("yes"))
              </MButton>
              <MButton variant=Flat onClick=(_e => send(Cancel))>
                (text("No"))
              </MButton>
            </MTypography>,
          |]
        | (EndorsementSubmited, _) => [|
            <CommandExecutor.Status
              cmdStatus
              action=Endorsement
              onRetry=(() => send(Cancel))
            />,
          |]
        | (RejectionSubmited, _) => [|
            <CommandExecutor.Status
              cmdStatus
              action=Rejection
              onRetry=(() => send(Cancel))
            />,
          |]
        },
      |]),
    ),
};
