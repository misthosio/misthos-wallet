include ViewCommon;

let component = ReasonReact.statelessComponent("ViewIncomeModal");

let make = (~signTAC, _children) => {
  let onAggree = event => {
    ReactEventRe.Synthetic.preventDefault(event);
    signTAC(TACText.hash);
  };
  {
    ...component,
    render: _ =>
      <div>
        <h1> ("Misthos terms and conditions" |> text) </h1>
        <MButton onClick=onAggree> ("Aggre" |> text) </MButton>
      </div>,
  };
};
