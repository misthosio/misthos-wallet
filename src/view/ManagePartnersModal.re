include ViewCommon;

open PrimitiveTypes;

type state = {
  viewData: ViewModel.ManagePartners.t,
  prospectId: string,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | RemovePartner(UserId.t);

let component = ReasonReact.reducerComponent("ManagePartners");

let make =
    (
      ~joinVentureUrl: string,
      ~viewData: ViewModel.ManagePartners.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {prospectId: "", viewData},
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({...state, prospectId: text})
    | ProposePartner =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        commands.proposePartner(~prospectId=prospectId |> UserId.fromString);
        ReasonReact.Update({...state, prospectId: ""});
      }
    | RemovePartner(partnerId) =>
      commands.proposePartnerRemoval(~partnerId);
      ReasonReact.NoUpdate;
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.array(
        Array.of_list(
          state.viewData.partners
          |> List.map((partner: ViewModel.partner) =>
               <Partner key=(partner.userId |> UserId.toString) partner />
             ),
        ),
      );
    let partnersOld =
      ReasonReact.array(
        Array.of_list(
          state.viewData.partners
          |> List.map((m: ViewModel.partner) =>
               <li key=(m.userId |> UserId.toString)>
                 <div>
                   (text(m.userId |> UserId.toString))
                   (
                     switch (m.canProposeRemoval) {
                     | true =>
                       <button onClick=(_e => send(RemovePartner(m.userId)))>
                         (text("Propose Removal"))
                       </button>
                     | _ => ReasonReact.null
                     }
                   )
                 </div>
               </li>
             ),
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
            value=(`String(state.prospectId))
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
          <MTypography variant=`Body2> (joinVentureUrl |> text) </MTypography>
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
          <ul> partnersOld </ul>
        </div>
    />;
  },
};
