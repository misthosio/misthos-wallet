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
  let gray = style([color(Colors.clickableGray)]);
  let inlineConfirm = style([display(`flex), alignItems(`baseline)]);
  let warning = style([color(Colors.error)]);
};

let make =
    (
      ~endorseText,
      ~alertText=?,
      ~rejectText,
      ~canVote,
      ~onEndorse,
      ~onReject,
      ~onCancel,
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
        (_ => onEndorse()),
      )
    | ConfirmReject =>
      ReasonReact.UpdateWithSideEffects(
        {...state, buttonState: RejectionSubmited},
        (_ => onReject()),
      )
    | Cancel =>
      ReasonReact.UpdateWithSideEffects(
        {cmdStatus: Idle, buttonState: NoDecision},
        (_ => onCancel()),
      )
    },
  render: ({send, state: {buttonState: state, cmdStatus}}) =>
    <div>
      (
        ReasonReact.array(
          Array.concatMany([|
            switch (cmdStatus, state, canVote) {
            | (_, NoDecision, false) => [|ReasonReact.null|]
            | (_, ConfirmReject, _) => [|
                <MTypography className=Styles.inlineConfirm variant=`Body2>
                  (rejectText |> text)
                  <MButton
                    gutterTop=false
                    variant=Flat
                    onClick=(_e => send(ConfirmReject))>
                    (text("yes"))
                  </MButton>
                  <MButton
                    gutterTop=false variant=Flat onClick=(_e => send(Cancel))>
                    (text("No"))
                  </MButton>
                </MTypography>,
              |]
            | (_, ConfirmEndorse, _) => [|
                <MTypography className=Styles.warning variant=`Body2>
                  (alertText |> Js.Option.getWithDefault("") |> text)
                </MTypography>,
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
            | (Idle, _, true)
            | (_, NoDecision, true) => [|
                <MButton fullWidth=true onClick=(_e => send(Endorse))>
                  (text(endorseText))
                </MButton>,
                <MButton
                  className=Styles.gray
                  variant=Flat
                  onClick=(_e => send(Reject))>
                  (text(rejectText))
                </MButton>,
              |]
            | (PreSubmitError(_) | Error(_), RejectionSubmited, _) => [|
                <CommandExecutor.Status cmdStatus action=Endorsement />,
                <MTypography className=Styles.inlineConfirm variant=`Body2>
                  <MButton variant=Flat onClick=(_e => send(Cancel))>
                    (text("Try Again"))
                  </MButton>
                </MTypography>,
              |]
            | (PreSubmitError(_) | Error(_), EndorsementSubmited, _) => [|
                <CommandExecutor.Status cmdStatus action=Rejection />,
                <MButton variant=Flat onClick=(_e => send(Cancel))>
                  (text("Try Again"))
                </MButton>,
              |]
            | (_, EndorsementSubmited, _) => [|
                <CommandExecutor.Status cmdStatus action=Endorsement />,
              |]
            | (_, RejectionSubmited, _) => [|
                <CommandExecutor.Status cmdStatus action=Rejection />,
              |]
            },
          |]),
        )
      )
    </div>,
};
