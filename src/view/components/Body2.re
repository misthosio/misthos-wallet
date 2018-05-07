let component = ReasonReact.statelessComponent("Body2");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  let grid =
    cssUnsafe({
      "display": "grid",
      "gridGap": "0 20px",
      "gridTemplateAreas": {|"title title title title title" ". body1 . body2 ."|},
      "gridTemplateColumns": "minmax(0, 1fr) minmax(400px, 4fr) 2fr minmax(400px, 4fr) minmax(0, 1fr)",
      "gridTemplateRows": "min-content auto",
      "width": "100%",
    });
  let area = area => cssUnsafe({"gridArea": area});
};

let make = (~titles=[], ~body1, ~body2, _children) => {
  ...component,
  render: _self =>
    <div className=Styles.grid>
      <TitleBar className=(Styles.area("title"))>
        (titles |> List.map(title => Utils.text(title)))
      </TitleBar>
      <div className=(Styles.area("body1")) key="body1"> body1 </div>
      <div className=(Styles.area("body2")) key="body2"> body2 </div>
    </div>,
};
