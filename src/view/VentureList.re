include ViewCommon;

open PrimitiveTypes;

let component = ReasonReact.statelessComponent("VentureList");

module Styles = {
  open Css;
  let linkSelected = style([color(Colors.misthosTeal)]);
  let link = style([fontSize(px(16)), textDecoration(underline)]);
};

let make = (~selected=?, ~index, _children) => {
  ...component,
  render: _self => {
    let ventureList =
      switch (index) {
      | None => <Spinner text="loading index" />
      | Some([]) =>
        <MTypography variant=`Body2>
          (
            {js|You are not part of any Ventures yet. Create a new Venture, or join an existing Venture if you have an invite link from a Partner.|js}
            |> text
          )
        </MTypography>
      | Some(index) =>
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
                       dense=true
                       button=true
                       value=(`String(ids))
                       onClick=(Router.clickToRoute(Venture(id, None)))
                       component=(`String("li"))>
                       <ListItemText
                         primary={
                           <MTypography
                             variant=`Title
                             className=(
                               Styles.link
                               ++ " "
                               ++ (
                                 Some(id) == selected ?
                                   Styles.linkSelected : ""
                               )
                             )>
                             (name |> text)
                           </MTypography>
                         }
                       />
                     </ListItem>;
                   }
                 ),
               ),
          ),
        )
      };
    <div>
      <MaterialUi.List dense=true disablePadding=true>
        ventureList
      </MaterialUi.List>
    </div>;
  },
};
