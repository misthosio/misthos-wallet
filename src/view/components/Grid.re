open Belt;

include ViewCommon;

type state = {activeTab: int};

type action =
  | ActiveTab(int);

let component = ReasonReact.reducerComponent("Grid");

type variant =
  | V1
  | V2
  | V3
  | V4;

module Styles = {
  open Css;
  open BreakPoints;
  let gapSM = (Theme.space(4) |> string_of_int) ++ "px 0px";
  let gapXS = (Theme.space(2) |> string_of_int) ++ "px 0px";
  let grid = (variant, warning) => {
    let warning = warning == None ? false : true;
    style([
      display(grid),
      sm([
        unsafe("gridGap", gapSM),
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
          | V3 =>
            {|
           ". title1 . title2 ."
           ". area3 . area4 ."
           ". area5 area5 area5 ."
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
          | V3
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
          | V3 =>
            "[tBegin] min-content [tEnd] auto min-content"
            ++ (warning ? " [wBegin] min-content [wEnd]" : "")
          | V2
          | V1 =>
            "[tBegin] min-content [tEnd] auto"
            ++ (warning ? " [wBegin] min-content [wEnd]" : "")
          },
        ),
      ]),
      xs([
        unsafe("gridGap", gapXS),
        unsafe(
          "gridTemplateAreas",
          switch (variant) {
          | V4 =>
            (warning ? {|". warning  ."|} : "")
            ++ {|
                ". area1 ."
                ". area2 ."
                ". tabs ."
                ". area3 . "
                ". area4 ."
                |}
          | V2 =>
            {|
               ". tabs ."
               ". area3 . "
               ". area4 ."
               |}
            ++ (warning ? {|" . warning  ."|} : "")
          | V3 =>
            {|
               ". tabs ."
               ". area3 . "
               ". area4 ."
               ". area5 ."
               |}
            ++ (warning ? {|" . warning ."|} : "")
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
          | V3
          | V2
          | V1 => "[begin] 16px minmax(100px, 9fr) 16px [end]"
          },
        ),
        unsafe(
          "gridTemplateRows",
          switch (variant) {
          | V4 =>
            (warning ? "[wBegin] min-content [wEnd] " : "")
            ++ "min-content min-content [tBegin] "
            ++ "min-content [tEnd] min-content min-content "
          | V3 =>
            "[tBegin] min-content [tEnd] min-content min-content"
            ++ (warning ? " [wBegin] min-content [wEnd]" : "")
          | V2
          | V1 =>
            "[tBegin] min-content [tEnd] min-content"
            ++ (warning ? " [wBegin] min-content [wEnd]" : "")
          },
        ),
      ]),
      width(`percent(100.0)),
      height(`percent(100.0)),
    ]);
  };
  let area = area => style([unsafe("gridArea", area), minHeight(px(0))]);

  let mobileHidden = hidden =>
    style([sm([display(block)]), xs([display(hidden ? none : block)])]);
  let title =
    style([
      fontFamily(Theme.oswald),
      height(px(45)),
      fontSize(px(30)),
      fontWeight(600),
      color(Colors.white),
      textTransform(uppercase),
      marginBottom(px(4)),
      sm([display(inline)]),
      xs([display(none)]),
    ]);
  let tabs =
    style([
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "tBegin / tEnd"),
      fontFamily(Theme.oswald),
      height(px(45)),
      fontSize(px(30)),
      fontWeight(600),
      color(Colors.white),
      textTransform(uppercase),
      sm([display(none)]),
    ]);
  let titleBg =
    style([
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "tBegin / tEnd"),
      backgroundColor(Colors.black),
      borderBottomStyle(solid),
      sm([
        unsafe("borderImageSlice", "1"),
        unsafe("borderImageSource", Colors.uGradient),
        unsafe("borderWidth", "0px 0px 4px 0px"),
      ]),
    ]);

  let warningBg =
    style([
      unsafe("gridColumn", "begin / end"),
      unsafe("gridRow", "wBegin / wEnd"),
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
      ~area5=?,
      ~warning=?,
      _children,
    ) => {
  ...component,
  initialState: () => {activeTab: 0},
  reducer: (action, _) =>
    switch (action) {
    | ActiveTab(i) => ReasonReact.Update({activeTab: i})
    },
  render: ({state, send}) => {
    let tabs =
      MaterialUi.(
        <Tabs
          onChange=((_, i) => ActiveTab(i) |> send)
          value=state.activeTab
          className=Styles.tabs
          fullWidth=true
          scrollable=true>
          <Tab label=(title1 |> Js.Option.getWithDefault(ReasonReact.null)) />
          <Tab label=(title2 |> Js.Option.getWithDefault(ReasonReact.null)) />
        </Tabs>
      );
    let variant =
      switch (area1, area3, area4, area5) {
      | (Some(_), Some(_), Some(_), _) => V4
      | (None, Some(_), Some(_), None) => V2
      | (None, Some(_), Some(_), Some(_)) => V3
      | (None, Some(_), None, None) => V1
      | (_, _, _, _) => V4
      };
    <div className=(Styles.grid(variant, warning))>
      (
        switch (warning) {
        | Some(_) =>
          <div
            className=(
              WarningBanner.Styles.warningBg ++ " " ++ Styles.warningBg
            )
            key="warningBg"
          />
        | None => ReasonReact.null
        }
      )
      <div className=Styles.titleBg key="titleBg" />
      (
        [|
          [|
            (warning, "warning", WarningBanner.Styles.warning(~inline=false)),
            (area1, "area1", ""),
            (area2, "area2", ""),
            (title1, "title1", Styles.title),
            (title2, "title2", Styles.title),
            (area3, "area3", Styles.mobileHidden(state.activeTab != 0)),
            (area4, "area4", Styles.mobileHidden(state.activeTab != 1)),
            (area5, "area5", ""),
          |]
          |. Array.map(((item, area, className)) =>
               switch (item) {
               | Some(item) =>
                 <div
                   className=(Styles.area(area) ++ " " ++ className) key=area>
                   item
                 </div>
               | None => ReasonReact.null
               }
             ),
          [|tabs|],
        |]
        |> Array.concatMany
        |> ReasonReact.array
      )
    </div>;
  },
};
