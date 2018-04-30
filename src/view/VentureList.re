open PrimitiveTypes;

let component = ReasonReact.statelessComponent("VentureList");

module Styles = {
  open Css;
  let linkSelected =
    style([
      display(block),
      width(`percent(100.0)),
      color(Colors.misthosTeal),
    ]);
  let link =
    style([display(block), width(`percent(100.0)), color(`currentColor)]);
  let title = style([padding2(~v=px(7), ~h=px(20))]);
};

let make = (~selected=?, ~index, _children) => {
  ...component,
  render: _self => {
    let ventureList =
      ReasonReact.array(
        Array.of_list(
          index
          |> List.map(
               Venture.Index.(
                 ({name, id}) => {
                   open MaterialUi;
                   let ids = id |> VentureId.toString;
                   <ListItem
                     key=ids
                     button=false
                     value=(`String(ids))
                     component=(`String("li"))>
                     <ListItemText
                       primary={
                         <Link
                           className=(
                             Some(id) == selected ?
                               Styles.linkSelected : Styles.link
                           )
                           route=(Venture(id))>
                           (name |> Utils.text)
                         </Link>
                       }
                     />
                   </ListItem>;
                 }
               ),
             ),
        ),
      );
    let _status = Utils.text("ventures:");
    let title =
      MaterialUi.(
        <TitleBar>
          <Typography key="titleBar" className=Styles.title variant=`Headline>
            ("My Ventures" |> Utils.text)
          </Typography>
        </TitleBar>
      );
    <div>
      title
      <MaterialUi.List dense=true> ventureList </MaterialUi.List>
    </div>;
  },
};
