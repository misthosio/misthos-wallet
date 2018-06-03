include ViewCommon;

open PrimitiveTypes;

module ViewData = ViewModel.ManagePartnersView;

type inputs = {
  prospectId: string,
  removePartnerId: option(UserId.t),
};

type state = {
  viewData: ViewData.t,
  canSubmitProposal: bool,
  removeInputFrozen: bool,
  inputs,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | SelectRemovePartner(UserId.t)
  | RemovePartner
  | AddAnother
  | FreezeRemoval
  | ResetRemoval;

let component = ReasonReact.reducerComponent("ManagePartners");

module Styles = {
  open Css;
  let icon = style([marginLeft(px(Theme.space(-1))), height(px(44))]);
  let stepper = style([padding2(~h=px(Theme.space(1)), ~v=px(0))]);
  let stepIconText =
    style([
      fontFamily(Theme.sourceSansPro),
      fontWeight(600),
      fontSize(px(18)),
      fontStyle(normal),
      lineHeight(1.0),
      letterSpacing(px(1)),
      unsafe("fill", "#" ++ Colors.uBlack),
    ]);
};

let make =
    (
      ~viewData: ViewData.t,
      ~proposePartnerCmds: CommandExecutor.commands,
      ~proposeCmdStatus: CommandExecutor.cmdStatus,
      ~removePartnerCmds: CommandExecutor.commands,
      ~removeCmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {
    inputs: {
      removePartnerId: None,
      prospectId: "",
    },
    removeInputFrozen: false,
    canSubmitProposal: false,
    viewData,
  },
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({
        ...state,
        canSubmitProposal: text != "",
        inputs: {
          ...state.inputs,
          prospectId: text,
        },
      })
    | ProposePartner =>
      switch (String.trim(state.inputs.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        proposePartnerCmds.proposePartner(
          ~prospectId=prospectId |> UserId.fromString,
        );
        ReasonReact.NoUpdate;
      }
    | RemovePartner =>
      state.inputs.removePartnerId
      |> Utils.mapOption(partnerId =>
           removePartnerCmds.proposePartnerRemoval(~partnerId)
         )
      |> ignore;
      ReasonReact.Update({
        ...state,
        inputs: {
          ...state.inputs,
          removePartnerId: None,
        },
      });
    | SelectRemovePartner(partner) =>
      switch (removeCmdStatus, state.removeInputFrozen) {
      | (Success(_) | Error(_), _)
      | (Idle, false) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            removeInputFrozen: false,
            inputs: {
              ...state.inputs,
              removePartnerId: Some(partner),
            },
          },
          ((_) => removePartnerCmds.reset()),
        )
      | _ => ReasonReact.NoUpdate
      }
    | FreezeRemoval => ReasonReact.Update({...state, removeInputFrozen: true})
    | ResetRemoval =>
      ReasonReact.UpdateWithSideEffects(
        {...state, removeInputFrozen: false},
        ((_) => removePartnerCmds.reset()),
      )
    | AddAnother =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          inputs: {
            ...state.inputs,
            prospectId: "",
          },
        },
        ((_) => proposePartnerCmds.reset()),
      )
    },
  render: ({send, state: {canSubmitProposal, viewData, inputs}}) => {
    let activeStep =
      switch (proposeCmdStatus) {
      | Success(_) => 1
      | _ => 0
      };
    let partners =
      ReasonReact.array(
        Array.of_list(
          viewData.partners
          |. Belt.List.keepMapU((. partner: ViewData.partner) =>
               partner.canProposeRemoval ?
                 Some(
                   <Partner
                     key=(partner.userId |> UserId.toString)
                     partnerId=partner.userId
                     name=?partner.name
                     onClick=(
                       _e => send(SelectRemovePartner(partner.userId))
                     )
                     button=MaterialUi.(
                              <Radio
                                color=`Primary
                                onChange=(
                                  (_e, _b) =>
                                    send(SelectRemovePartner(partner.userId))
                                )
                                checked=(
                                          `Bool(
                                            inputs.removePartnerId
                                            == Some(partner.userId),
                                          )
                                        )
                              />
                            )
                   />,
                 ) :
                 None
             ),
        ),
      );
    let icon = index =>
      <svg width="44" height="44" viewBox="0 0 44 44">
        <defs>
          <linearGradient
            id="a" x1="162.467%" x2="-41.102%" y1="29.557%" y2="66.287%">
            <stop offset="0%" stopColor="#05CFDB" />
            <stop offset="100%" stopColor="#02A2B4" />
          </linearGradient>
        </defs>
        <g fill="none" fillRule="evenodd" transform="translate(1 1)">
          <circle cx="21" cy="21" r="21" stroke="#000" />
          <circle cx="21" cy="21" r="18" fill="url(#a)" />
          (
            if (index < activeStep) {
              <polyline
                fill="none"
                stroke="#000"
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                points="16 0 5 11 0 6"
                transform="translate(12 16)"
              />;
            } else {
              <text
                className=Styles.stepIconText
                x="21"
                y="27"
                textAnchor="middle">
                (index + 1 |> string_of_int |> text)
              </text>;
            }
          )
        </g>
      </svg>;
    <Grid
      title1=("Add a partner" |> text)
      title2=("Remove a partner" |> text)
      area3=
        <div>
          MaterialUi.(
            <Stepper
              className=Styles.stepper
              orientation=`Vertical
              activeStep=(`Int(activeStep))>
              <Step key="enter-id">
                <StepLabel
                  classes=[IconContainer(Styles.icon)] icon=(icon(0))>
                  ("ADD A BLOCKSTACK ID" |> text)
                </StepLabel>
                <StepContent>
                  <MInput
                    placeholder="Enter a Blockstack ID"
                    value=(`String(inputs.prospectId))
                    onChange=(
                      e => send(ChangeNewPartnerId(extractString(e)))
                    )
                    autoFocus=true
                    fullWidth=true
                  />
                  <ProposeButton
                    onSubmit=(() => send(ProposePartner))
                    canSubmitProposal
                    withConfirmation=false
                    proposeText="Propose partner"
                    cmdStatus=proposeCmdStatus
                  />
                </StepContent>
              </Step>
              <Step>
                <StepLabel
                  classes=[IconContainer(Styles.icon)] icon=(icon(1))>
                  ("SHARE THE VENTURE URL" |> text)
                </StepLabel>
                <StepContent>
                  <MTypography variant=`Body2>
                    (
                      {js|
               Please send the following URL to the proposed Partner so they can access the Venture:
               |js}
                      |> text
                    )
                  </MTypography>
                  <MButton variant=Flat onClick=(_e => send(AddAnother))>
                    (text("Add Another"))
                  </MButton>
                </StepContent>
              </Step>
            </Stepper>
          )
          <MTypography variant=`Body2>
            (viewData.joinVentureUrl |> text)
          </MTypography>
        </div>
      area4=
        <div>
          <MTypography variant=`Body2>
            (
              {js|
               To propose the removal of a partner from this venture,
               select his or her name below and submit your proposal.
               When enough partners endorse this proposal, the partner will be removed.
               |js}
              |> text
            )
          </MTypography>
          <ScrollList>
            <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          </ScrollList>
          <ProposeButton
            onPropose=(() => send(FreezeRemoval))
            onSubmit=(() => send(RemovePartner))
            onCancel=(() => send(ResetRemoval))
            canSubmitProposal=(inputs.removePartnerId |> Js.Option.isSome)
            proposeText="Propose partner removal"
            cmdStatus=removeCmdStatus
          />
        </div>
    />;
  },
};
