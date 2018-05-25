include ViewCommon;

open PrimitiveTypes;

[@bs.module] external remove : string = "../assets/img/remove-partner.svg";

[@bs.module] external stepBg : string = "../assets/img/step_bg.svg";

module ViewData = ViewModel.ManagePartnersView;

type inputs = {prospectId: string};

type state = {
  viewData: ViewData.t,
  activeStep: int,
  inputs,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | RemovePartner(UserId.t)
  | ProposePartnerSuccess;

let component = ReasonReact.reducerComponent("ManagePartners");

module Styles = {
  open Css;
  let lenght = px(Theme.space(3));
  let stepLabel =
    style([
      backgroundImage(url(stepBg)),
      backgroundSize(`size((lenght, lenght))),
      height(lenght),
      width(lenght),
      marginRight(px(Theme.space(1))),
    ]);
};

let make =
    (
      ~viewData: ViewData.t,
      ~commands: CommandExecutor.commands,
      ~cmdStatus: CommandExecutor.cmdStatus,
      _children,
    ) => {
  ...component,
  initialState: () => {
    inputs: {
      prospectId: "",
    },
    activeStep: 0,
    viewData,
  },
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({
        ...state,
        inputs: {
          prospectId: text,
        },
      })
    | ProposePartner =>
      switch (String.trim(state.inputs.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        commands.proposePartner(~prospectId=prospectId |> UserId.fromString);
        ReasonReact.NoUpdate;
      }
    | RemovePartner(partnerId) =>
      commands.proposePartnerRemoval(~partnerId);
      ReasonReact.NoUpdate;
    | ProposePartnerSuccess =>
      ReasonReact.Update({
        ...state,
        inputs: {
          prospectId: "",
        },
        activeStep: 1,
      })
    },
  render: ({send, state: {viewData, inputs, activeStep}}) => {
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
                     button=MaterialUi.(
                              <IconButton
                                onClick=(
                                  _e => send(RemovePartner(partner.userId))
                                )>
                                <img src=remove alt="Remove" />
                              </IconButton>
                            )
                   />,
                 ) :
                 None
             ),
        ),
      );
    let onSuccess = () => send(ProposePartnerSuccess);
    <Body2
      titles=["Add a partner", "Remove a partner"]
      body1=
        <div>
          MaterialUi.(
            <Stepper orientation=`Vertical activeStep=(`Int(activeStep))>
              <Step key="enter-id">
                <StepLabel classes=[IconContainer(Styles.stepLabel)]>
                  ("ADD A BLOCKSTACK ID" |> text)
                </StepLabel>
                <StepContent>
                  <MInput
                    placeholder="Enter a Blockstack ID"
                    value=(`String(inputs.prospectId))
                    onChange=(
                      e => send(ChangeNewPartnerId(extractString(e)))
                    )
                    autoFocus=false
                    fullWidth=true
                  />
                  <MButton
                    fullWidth=true onClick=(_e => send(ProposePartner))>
                    (text("Propose partner addition"))
                  </MButton>
                  <CommandExecutor.Status
                    onSuccess
                    cmdStatus
                    action=Proposal
                  />
                </StepContent>
              </Step>
              <Step>
                <StepLabel> ("SHARE THE VENTURE URL" |> text) </StepLabel>
                <StepContent>
                  <MTypography variant=`Body2>
                    (
                      {js|
               Please send the following URL to the proposed Partner so they can access the Venture:
               |js}
                      |> text
                    )
                  </MTypography>
                  <MTypography variant=`Body2>
                    (viewData.joinVentureUrl |> text)
                  </MTypography>
                </StepContent>
              </Step>
            </Stepper>
          )
        </div>
      body2=
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
          <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
        </div>
    />;
  },
};
