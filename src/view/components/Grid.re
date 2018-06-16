let component = ReasonReact.statelessComponent("Grid");

[@bs.module "glamor"] external cssUnsafe : Js.t({..}) => string = "css";

type variant =
  | V1
  | V2
  | V4;

module Styles = {
  open Css;
  let gap = (Theme.space(4) |> string_of_int) ++ "px 0px";
  let grid = (variant, warning) => {
    let warning = warning == None ? false : true;
    style([
      display(grid),
      unsafe("gridGap", gap),
      unsafe(
        "gridTemplateAreas",
        switch (variant) {
        | V4 =>
          (warning ? {|" . warning warning warning ."|} : "")
          ++ {|
              ". area1 . area2 ."
              ". title1 . title2 ."
              ". area3 . area4 ."
              |}
        | V2 =>
          {|
              ". title1 . title2 ."
              ". area3 . area4 ."
           |}
          ++ (warning ? {|" . warning warning warning ."|} : "")
        | V1 =>
          {|
              ". title1 ."
              ". area3 ."
              |}
          ++ (warning ? {|". warning ."|} : "")
        },
      ),
      unsafe(
        "gridTemplateColumns",
        switch (variant) {
        | V4
        | V2 => "[begin] minmax(24px, 1fr) minmax(368px, 4fr) minmax(24px, 1fr) minmax(368px, 4fr) minmax(24px, 1fr) [end]"
        | V1 => "[begin] minmax(24px, 1fr) minmax(368px, 9fr) minmax(24px, 1fr) [end]"
        },
      ),
      unsafe(
        "gridTemplateRows",
        switch (variant) {
        | V4 =>
          (warning ? "[wBegin] min-content [wEnd] " : "")
          ++ "min-content [tBegin] min-content [tEnd] auto"
        | V2
        | V1 =>
          "[tBegin] min-content [tEnd] auto"
          ++ (warning ? " [wBegin] min-content [wEnd]" : "")
        },
      ),
      width(`percent(100.0)),
      height(`percent(100.0)),
    ]);
  };
  let area = area => style([unsafe("gridArea", area), minHeight(px(0))]);
  let warning =
    style([
      fontFamily(Theme.sourceSansPro),
      height(px(36)),
      fontSize(px(14)),
      fontWeight(700),
      color(Colors.white),
      textTransform(uppercase),
      padding2(~h=px(0), ~v=px(Theme.space(1))),
      children([
        selector(
          ":any-link,:-webkit-any-link",
          [
            color(Colors.white),
            unsafe("textDecorationColor", Colors.uWhite),
            hover([color(Colors.misthosTeal)]),
          ],
        ),
      ]),
    ]);
  let warningBg =
    style([
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "wBegin / wEnd"),
      unsafe("background", Colors.uGradientOrange),
    ]);
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
      unsafe("gridRow", "tBegin / tEnd"),
      backgroundColor(Colors.black),
      borderBottomStyle(solid),
      unsafe("borderImageSlice", "1"),
      unsafe("borderImageSource", Colors.uGradient),
      unsafe("borderWidth", "0px 0px 4px 0px"),
    ]);
};

let make =
    (
      ~title1=?,
      ~title2=?,
      ~area1=?,
      ~area2=?,
      ~area3=?,
      ~area4=?,
      ~warning=?,
      _children,
    ) => {
  ...component,
  render: _self => {
    let variant =
      switch (area1, area3, area4) {
      | (Some(_), Some(_), Some(_)) => V4
      | (None, Some(_), Some(_)) => V2
      | (None, Some(_), None) => V1
      | (_, _, _) => V4
      };
    <div className=(Styles.grid(variant, warning))>
      (
        switch (warning) {
        | Some(_) => <div className=Styles.warningBg key="warningBg" />
        | None => ReasonReact.null
        }
      )
      <div className=Styles.titleBg key="titleBg" />
      (
        [|
          (warning, "warning", Styles.warning),
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
