let component = ReasonReact.statelessComponent("Grid");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

module Styles = {
  open Css;
  let gap = (Theme.space(8) |> string_of_int) ++ "px";
  let grid = firstRow =>
    cssUnsafe({
      "display": "grid",
      "gridGap": gap ++ " " ++ gap,
      "gridTemplateAreas":
        switch (firstRow) {
        | Some(_) => {|
                       ". area1 . area2 ."
                       ". title1 . title2 ."
                       ". area3 . area4 ."
                       |}
        | None => {|
                       ". title1 . title2 ."
                       ". area3 . area4 ."
                   |}
        },
      "gridTemplateColumns": "[begin] minmax(0, 1fr) minmax(400px, 4fr) 1fr minmax(400px, 4fr) minmax(0, 1fr) [end]",
      "gridTemplateRows":
        switch (firstRow) {
        | Some(_) => "min-content [begin] min-content [end] auto"
        | None => "[begin] min-content [end] auto"
        },
      "width": "100%",
    });
  let area = area => style([unsafe("gridArea", area)]);
  let title =
    style([
      fontFamily(Theme.oswald),
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
      borderBottomWidth(px(4)),
      borderBottomStyle(solid),
      unsafe("borderImageSlice", "1"),
      unsafe("borderImageSource", Colors.uGradient),
    ]);
};

let make =
    (~title1=?, ~title2=?, ~area1=?, ~area2=?, ~area3=?, ~area4=?, _children) => {
  ...component,
  render: _self =>
    <div className=(Styles.grid(area1))>
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
    </div>,
};
