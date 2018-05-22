include ViewCommon;

open PrimitiveTypes;

[@bs.module] external remove : string = "../assets/img/remove-partner.svg";

module ViewData = ViewModel.ViewPartnerView;

type inputs = {prospectId: string};

type state = {
  viewData: ViewData.t,
  inputs,
};

type action =
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
    "hello" |> text;
  },
};
