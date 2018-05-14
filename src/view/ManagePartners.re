open PrimitiveTypes;

let text = Utils.text;

type state = {
  viewModel: ViewModel.t,
  prospectId: string,
};

type action =
  | ChangeNewPartnerId(string)
  | ProposePartner
  | RemovePartner(UserId.t);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
  );

let component = ReasonReact.reducerComponent("ManagePartners");

let make =
    (
      ~venture as initialViewModel,
      ~session: Session.Data.t,
      ~commands: VentureWorkerClient.Cmd.t,
      _children,
    ) => {
  ...component,
  initialState: () => {viewModel: initialViewModel, prospectId: ""},
  willReceiveProps: ({state}) => {...state, viewModel: initialViewModel},
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
          ViewModel.partners(state.viewModel)
          |> List.map((partner: ViewModel.partner) =>
               <Partner key=(partner.userId |> UserId.toString) partner />
             ),
        ),
      );
    let partnersOld =
      ReasonReact.array(
        Array.of_list(
          ViewModel.partners(state.viewModel)
          |> List.map((m: ViewModel.partner) =>
               <li key=(m.userId |> UserId.toString)>
                 <div>
                   (text(m.userId |> UserId.toString))
                   (
                     switch (
                       m.userId |> UserId.eq(session.userId),
                       ViewModel.removalProspects(state.viewModel)
                       |> List.exists((p: ViewModel.prospect) =>
                            UserId.eq(p.userId, m.userId)
                          ),
                     ) {
                     | (false, false) =>
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
              |> Utils.text
            )
          </MTypography>
          <MInput
            placeholder="Enter a Blockstack ID"
            value=(`String(state.prospectId))
            onChange=(e => send(changeNewPartnerId(e)))
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
              |> Utils.text
            )
          </MTypography>
          <MTypography variant=`Body2>
            (
              Location.origin
              ++ Router.Config.routeToUrl(
                   JoinVenture(initialViewModel.ventureId, session.userId),
                 )
              |> Utils.text
            )
          </MTypography>
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
              |> Utils.text
            )
          </MTypography>
          <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          <ul> partnersOld </ul>
        </div>
    />;
  },
};
