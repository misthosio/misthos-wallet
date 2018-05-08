let component = ReasonReact.statelessComponent("TitleBar");

/* TODO switch to bs-css when all grid features we need are supported */
[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  open Css;
  let barGrid =
    cssUnsafe({
      "display": "grid",
      "gridGap": "0 20px",
      "gridTemplateAreas": {|". title1 . title2 ." "line line line line line"|},
      "gridTemplateColumns": "minmax(0, 1fr) minmax(400px, 4fr) 1fr minmax(400px, 4fr) minmax(0, 1fr)",
      "gridTemplateRows": "auto 4px",
      "width": "100%",
    });
  let bar = style([backgroundColor(Colors.black), display(grid)]);
  let title = style([padding2(~v=px(7), ~h=px(0))]);
  let gradient = style([height(px(4)), backgroundImage(Colors.gradient)]);
  let area = area => cssUnsafe({"gridArea": area});
};

let make = (~className="", ~titles=[], _children) => {
  ...component,
  render: _self =>
    <div className=(Styles.bar ++ " " ++ Styles.barGrid ++ " " ++ className)>
      (
        titles
        |> List.mapi((i, title) => {
             let si = i + 1 |> string_of_int;
             <div
               key=si
               className=(Styles.area("title" ++ si) ++ " " ++ Styles.title)>
               <MaterialUi.Typography variant=`Headline>
                 (title |> Utils.text)
               </MaterialUi.Typography>
             </div>;
           })
        |> Array.of_list
        |> ReasonReact.array
      )
      <div
        key="line"
        className=(Styles.gradient ++ " " ++ Styles.area("line"))
      />
    </div>,
};
