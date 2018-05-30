let component = ReasonReact.statelessComponent("Body2");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  let grid =
    cssUnsafe({
      "display": "grid",
      "gridGap":
        (Theme.space(8) |> string_of_int)
        ++ "px "
        ++ (Theme.space(3) |> string_of_int)
        ++ "px",
      "gridTemplateAreas": {|"title title title title title" ". body1 . body2 ."|},
      "gridTemplateColumns": "minmax(0, 1fr) minmax(400px, 4fr) 1fr minmax(400px, 4fr) minmax(0, 1fr)",
      "gridTemplateRows": "min-content 65vh",
      "width": "100%",
    });
  let area = area => cssUnsafe({"gridArea": area});
};

let make = (~titles=[], ~body1, ~body2, _children) => {
  ...component,
  render: _self =>
    <div className=Styles.grid>
      <TitleBar className=(Styles.area("title")) titles />
      <div className=(Styles.area("body1")) key="body1"> body1 </div>
      <div className=(Styles.area("body2")) key="body2"> body2 </div>
    </div>,
};
