let component = ReasonReact.statelessComponent("Grid");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

type variant =
  | V1
  | V2
  | V4;

module Styles = {
  open Css;
  let gap = (Theme.space(8) |> string_of_int) ++ "px";
  let grid = variant =>
    cssUnsafe({
      "display": "grid",
      "gridGap": gap ++ " " ++ gap,
      "gridTemplateAreas":
        switch (variant) {
        | V4 => {|
                 ". area1 . area2 ."
                 ". title1 . title2 ."
                 ". area3 . area4 ."
                 |}
        | V2 => {|
                 ". title1 . title2 ."
                 ". area3 . area4 ."
                 |}
        | V1 => {|
                 ". title1 title1 title1 ."
                 ". area3 area3 area3 ."
                 |}
        },
      "gridTemplateColumns": "[begin] minmax(0, 1fr) minmax(400px, 4fr) 1fr minmax(400px, 4fr) minmax(0, 1fr) [end]",
      "gridTemplateRows":
        switch (variant) {
        | V4 => "min-content [begin] min-content [end] auto"
        | V2
        | V1 => "[begin] min-content [end] auto"
        },
      "width": "100%",
    });
  let area = area => style([unsafe("gridArea", area)]);
  let title =
    style([
      fontFamily(Theme.oswald),
      height(px(45)),
      fontSize(px(30)),
      fontWeight(600),
      color(Colors.white),
      textTransform(uppercase),
      marginBottom(px(4)),
    ]);
  let titleBg =
    style([
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "begin / end"),
      backgroundColor(Colors.black),
      borderBottomStyle(solid),
      unsafe("borderImageSlice", "1"),
      unsafe("borderImageSource", Colors.uGradient),
      unsafe("borderWidth", "0px 0px 4px 0px"),
    ]);
};

let make =
    (~title1=?, ~title2=?, ~area1=?, ~area2=?, ~area3=?, ~area4=?, _children) => {
  ...component,
  render: _self => {
    let variant =
      switch (area1, area3, area4) {
      | (Some(_), Some(_), Some(_)) => V4
      | (None, Some(_), Some(_)) => V2
      | (None, Some(_), None) => V1
      | (_, _, _) => V4
      };
    <div className=(Styles.grid(variant))>
      <div className=Styles.titleBg key="titleBg" />
      (
        [|
          (area1, "area1", ""),
          (area2, "area2", ""),
          (title1, "title1", Styles.title),
          (title2, "title2", Styles.title),
          (area3, "area3", ""),
          (area4, "area4", ""),
        |]
        |> Array.map(((item, area, className)) =>
             switch (item) {
             | Some(item) =>
               <div
                 className=(Styles.area(area) ++ " " ++ className) key=area>
                 item
               </div>
             | None => ReasonReact.null
             }
           )
        |> ReasonReact.array
      )
    </div>;
  },
};
