include ViewCommon;

let component = ReasonReact.statelessComponent("TitleBar");

module Styles = {
  open Css;
  let bar = (titlesCount, gap) =>
    style([
      display(grid),
      unsafe("gridGap", gap ? "0 20px" : "0"),
      unsafe(
        "gridTemplateAreas",
        switch (titlesCount) {
        | 1 => {|". title1 title1 title1 ." "line line line line line"|}
        | _ => {|". title1 . title2 ." "line line line line line"|}
        },
      ),
      unsafe(
        "gridTemplateColumns",
        "minmax(0, 1fr) minmax(min-content, 4fr) 1fr minmax(min-content, 4fr) minmax(0, 1fr)",
      ),
      unsafe("gridTemplateRows", "auto 4px"),
      width(`percent(100.0)),
      backgroundColor(Colors.black),
      display(grid),
    ]);
  let title = style([padding2(~v=px(7), ~h=px(0))]);
  let gradient = style([height(px(4)), backgroundImage(Colors.gradient)]);
  let area = area => style([unsafe("gridArea", area)]);
};

let make = (~className="", ~titles=[], ~gap=true, _children) => {
  ...component,
  render: _self =>
    <div
      className=(Styles.bar(titles |> List.length, gap) ++ " " ++ className)>
      (
        titles
        |> List.mapi((i, title) => {
             let si = i + 1 |> string_of_int;
             <div
               key=si
               className=(Styles.area("title" ++ si) ++ " " ++ Styles.title)>
               <MaterialUi.Typography variant=`Headline>
                 (title |> text)
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
