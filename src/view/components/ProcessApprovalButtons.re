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
            <MButton fullWidth=true onClick=(_e => send(Reject))>
              (text(rejectText))
            </MButton>,
          |]
        | (NoDecision, false) => [|ReasonReact.null|]
        | (ConfirmReject, _) => [|
            text("Confirm your rejection"),
            <MButton fullWidth=true onClick=(_e => send(ConfirmReject))>
              (text("yes"))
            </MButton>,
            <MButton fullWidth=true onClick=(_e => send(Cancel))>
              (text("No"))
            </MButton>,
          |]
        | (ConfirmEndorse, _) => [|
            text("Confirm your endorsement "),
            <MButton fullWidth=true onClick=(_e => send(ConfirmEndorse))>
              (text("yes"))
            </MButton>,
            <MButton fullWidth=true onClick=(_e => send(Cancel))>
              (text("No"))
            </MButton>,
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
