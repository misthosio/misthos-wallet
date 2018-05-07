let component = ReasonReact.statelessComponent("TitleBar");

/* TODO switch to bs-css when all grid features we need are supported */
[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  open Css;
  let titleGrid =
    cssUnsafe({
      "display": "grid",
      "gridGap": "0 20px",
      "gridTemplateColumns": "minmax(0, 1fr) minmax(400px, 10fr) minmax(0, 1fr)",
      "gridTemplateRows": "auto 4px",
      "width": "100%",
    });
  let title = style([backgroundColor(Colors.black), display(grid)]);
  let container =
    cssUnsafe({
      "gridColumn": "2 / 3",
      "gridRow": "1",
      "display": "grid",
      "padding": "7px 0",
      "gridTemplateColumns": "repeat(auto-fit, minmax(400px, 1fr))",
    });
  let gradient = style([height(px(4)), backgroundImage(Colors.gradient)]);
  let gradientGrid = cssUnsafe({"gridColumn": "1 / 4", "gridRow": "2"});
};

let make = (~className="", children) => {
  ...component,
  render: _self =>
    <div
      className=(Styles.title ++ " " ++ Styles.titleGrid ++ " " ++ className)>
      <div className=Styles.container>
        (
          children
          |> Array.mapi((i, child) =>
               <MaterialUi.Typography
                 key=(i |> string_of_int) variant=`Headline>
                 child
               </MaterialUi.Typography>
             )
          |> ReasonReact.array
        )
      </div>
      <div className=(Styles.gradient ++ " " ++ Styles.gradientGrid) />
    </div>,
};
