include ViewCommon;

open PrimitiveTypes;

[@bs.module] external remove : string = "../assets/img/remove-partner.svg";

module ViewData = ViewModel.ManagePartnersView;

type inputs = {prospectId: string};

type state = {
  viewData: ViewData.t,
  inputs,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | RemovePartner(UserId.t);

let component = ReasonReact.reducerComponent("ManagePartners");

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
        ReasonReact.Update({
          ...state,
          inputs: {
            prospectId: "",
          },
        });
      }
    | RemovePartner(partnerId) =>
      commands.proposePartnerRemoval(~partnerId);
      ReasonReact.NoUpdate;
    },
  render: ({send, state: {viewData, inputs}}) => {
    let feedback =
      switch (cmdStatus) {
      | Pending(_) => <Spinner text="waiting for result" />
      | Error(_) => "Could not execute teh command" |> text
      | _ => ReasonReact.null
      };
    let partners =
      ReasonReact.array(
        Array.of_list(
          viewData.partners
          |> List.map((partner: ViewData.partner) => {
               let button =
                 partner.canProposeRemoval ?
                   Some(
                     MaterialUi.(
                       <IconButton
                         onClick=(_e => send(RemovePartner(partner.userId)))>
                         <img src=remove alt="Remove" />
                       </IconButton>
                     ),
                   ) :
                   None;
               <Partner
                 key=(partner.userId |> UserId.toString)
                 partnerId=partner.userId
                 name=?partner.name
                 ?button
               />;
             }),
        ),
      );
    <Body2
      titles=["Add a partner", "Remove a partner"]
      body1=
        <div>
          <MTypography variant=`Body2>
            (
              {js|
                 To propose adding a new partner to the venture,
                 enter a valid Blockstack ID below. When enough partners endorse this proposal,
                 the partner will be added.
                |js}
              |> text
            )
          </MTypography>
          <MInput
            placeholder="Enter a Blockstack ID"
            value=(`String(inputs.prospectId))
            onChange=(e => send(ChangeNewPartnerId(extractString(e))))
            autoFocus=false
            fullWidth=true
          />
          <MButton fullWidth=true onClick=(_e => send(ProposePartner))>
            (text("Propose partner addition"))
          </MButton>
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
          feedback
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
