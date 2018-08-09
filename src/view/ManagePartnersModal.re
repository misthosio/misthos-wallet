include ViewCommon;

[@bs.val] external encodeURI : string => string = "";

open PrimitiveTypes;

module ViewData = ViewModel.ManagePartnersView;

type inputs = {
  prospectId: string,
  removePartnerId: option(UserId.t),
};

type state = {
  viewData: ViewData.t,
  alertText: option(string),
  canSubmitProposal: bool,
  removeInputFrozen: bool,
  inputs,
  suggestions: Belt.Map.String.t(array(string)),
  displayedSuggestions: array(string),
};

type action =
  | UpdateSuggestions(string, array(string))
  | UpdateDisplayedSuggestions(string)
  | ClearSuggestions
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
  let autoCompleteContianerOverflow =
    style([children([important(overflow(visible))])]);
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
  let autoCompleteContainer = style([position(relative)]);
  let suggestionsContainerOpen =
    style([
      position(absolute),
      zIndex(2000),
      marginTop(px(Theme.space(1))),
      left(px(0)),
      right(px(0)),
    ]);

  let suggestion = style([display(block)]);
  let suggestionItem = style([fontSize(px(14))]);
  let suggestionsList =
    style([margin(px(0)), padding(px(0)), listStyleType(none)]);
  let ventureLink =
    style([
      textDecoration(underline),
      color(`currentColor),
      hover([color(Colors.misthosTeal)]),
    ]);
};

let renderInputComponent = props =>
  <MInput
    placeholder="Enter a Blockstack ID"
    value=(`String(props##value))
    onChange=props##onChange
    autoFocus=true
    fullWidth=true
    inputProps=props
  />;

let renderSuggestionsContainer =
    (
      options: {
        .
        "containerProps": Js.t({..}),
        "children": ReasonReact.reactElement,
      },
    ) =>
  ReasonReact.cloneElement(
    MaterialUi.(<Paper square=true />),
    ~props=options##containerProps,
    [|options##children|],
  );

let renderSuggestion = (suggestion, vals) => {
  let query = vals##query;
  let isHighlighted = vals##isHighlighted;
  let parts =
    AutosuggestHighlight.(match(suggestion, query) |> parse(suggestion));

  <MaterialUi.MenuItem
    className=Styles.suggestionItem
    selected=isHighlighted
    component=(`String("div"))>
    <div>
      (
        parts
        |. Belt.Array.mapWithIndexU((. index, part) =>
             part##highlight ?
               <span
                 className=Css.(style([fontWeight(600)]))
                 key=(string_of_int(index))>
                 (part##text |> text)
               </span> :
               <strong
                 className=Css.(style([fontWeight(300)]))
                 key=(string_of_int(index))>
                 (part##text |> text)
               </strong>
           )
        |> ReasonReact.array
      )
    </div>
  </MaterialUi.MenuItem>;
};

let filterSuggestions = (prospectId, suggestions) => {
  let inputLength = prospectId |> Js.String.length;
  inputLength < 3 ?
    [||] :
    suggestions
    |. Belt.Array.keepU((. s) =>
         s |> Js.String.slice(~from=0, ~to_=inputLength) == prospectId
       );
};

let addSuggestions = (suggestionsMap, query, suggestions) =>
  switch (suggestions) {
  | [||] => suggestionsMap
  | suggestions => Belt.(suggestionsMap |. Map.String.set(query, suggestions))
  };
let rec getSuggestions = (suggestions, query) =>
  Belt.(
    switch (query, suggestions |. Map.String.get(query)) {
    | (query, _) when query |> Js.String.length < 3 => [||]
    | (_, Some(suggestions)) => suggestions
    | (query, None) =>
      getSuggestions(
        suggestions,
        query
        |> Js.String.substring(~from=0, ~to_=Js.String.length(query) - 1),
      )
    }
  );

let onSuggestionsFetchRequested = (send, suggestions, arg) => {
  let query = arg##value;
  if (arg##reason == "input-changed") {
    send(UpdateDisplayedSuggestions(query));
  };
  Blockstack.fetchIds(
    ~current=getSuggestions(suggestions, query),
    arg##value,
  )
  |> Js.Promise.then_(((query, s)) =>
       send(UpdateSuggestions(query, s)) |> Js.Promise.resolve
     )
  |> ignore;
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
    alertText: None,
    inputs: {
      removePartnerId: None,
      prospectId: "",
    },
    removeInputFrozen: false,
    canSubmitProposal: false,
    viewData,
    suggestions: Belt.Map.String.empty,
    displayedSuggestions: [||],
  },
  willReceiveProps: ({state}) => {...state, viewData},
  reducer: (action, {viewData} as state) =>
    switch (action) {
    | UpdateSuggestions(query, suggestions) =>
      let suggestions = addSuggestions(state.suggestions, query, suggestions);
      ReasonReact.Update({
        ...state,
        suggestions,
        displayedSuggestions:
          filterSuggestions(
            state.inputs.prospectId,
            getSuggestions(suggestions, state.inputs.prospectId),
          ),
      });
    | UpdateDisplayedSuggestions(value) =>
      ReasonReact.Update({
        ...state,
        displayedSuggestions:
          filterSuggestions(value, getSuggestions(state.suggestions, value)),
      })

    | ClearSuggestions =>
      ReasonReact.Update({...state, displayedSuggestions: [||]})
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
      ReasonReact.Update({...state, alertText: None});
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
            alertText:
              viewData.alertPartners |. Belt.Set.has(partner) ?
                WarningsText.partnerRemovalRisk |. Some : None,
          },
          (_ => removePartnerCmds.reset()),
        )
      | _ => ReasonReact.NoUpdate
      }
    | FreezeRemoval => ReasonReact.Update({...state, removeInputFrozen: true})
    | ResetRemoval =>
      ReasonReact.UpdateWithSideEffects(
        {...state, removeInputFrozen: false},
        (_ => removePartnerCmds.reset()),
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
        (_ => proposePartnerCmds.reset()),
      )
    },
  render:
    (
      {
        send,
        state: {alertText, canSubmitProposal, viewData, inputs} as state,
      },
    ) => {
    let onSubmit = () => send(ProposePartner);
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

    <Grid
      title1=("Addition Proposal" |> text)
      title2=("Removal Proposal" |> text)
      area3={
        <form onSubmit=(ignoreEvent(onSubmit))>
          <MTypography variant=`Body2>
            ("Add a Blockstack ID" |> text)
          </MTypography>
          <Autosuggest
            theme={
              "container": Styles.autoCompleteContainer,
              "suggestionsContainerOpen": Styles.suggestionsContainerOpen,
              "suggestion": Styles.suggestion,
              "suggestionsList": Styles.suggestionsList,
            }
            suggestions=state.displayedSuggestions
            getSuggestionValue=(s => s)
            onSuggestionsFetchRequested=(
              onSuggestionsFetchRequested(send, state.suggestions)
            )
            shouldRenderSuggestions=(
              value => Js.String.trim(value) |> Js.String.length > 2
            )
            onSuggestionsClearRequested=(() => send(ClearSuggestions))
            renderSuggestion
            renderInputComponent
            renderSuggestionsContainer
            inputProps={
              "value": inputs.prospectId,
              "onChange": (_e, change) =>
                send(ChangeNewPartnerId(change##newValue)),
            }
          />
          <SingleActionButton
            onSubmit
            canSubmitAction=canSubmitProposal
            withConfirmation=false
            buttonText="Propose partner addition"
            cmdStatus=proposeCmdStatus
          />
        </form>
      }
      area4={
        <div className=ScrollList.containerStyles>
          <MTypography variant=`Body2>
            (
              {js|
               To propose the removal of a Partner from this Venture,
               select his or her name below and submit your proposal.
               When enough Partners endorse this proposal, the Partner will be removed.
               |js}
              |> text
            )
          </MTypography>
          <ScrollList>
            <MaterialUi.List disablePadding=true> partners </MaterialUi.List>
          </ScrollList>
          <SingleActionButton
            onPropose=(() => send(FreezeRemoval))
            onSubmit=(() => send(RemovePartner))
            onCancel=(() => send(ResetRemoval))
            canSubmitAction=(inputs.removePartnerId |> Js.Option.isSome)
            buttonText="Propose Partner Removal"
            ?alertText
            cmdStatus=removeCmdStatus
          />
        </div>
      }
    />;
  },
};
