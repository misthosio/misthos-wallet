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
    let ventures =
      index |> Utils.mapOption(({ventures}: Venture.Index.t) => ventures);
    let breakingChange =
      index
      |> Utils.mapOption(({breakingChange}: Venture.Index.t) =>
           breakingChange
         );
    let breakingNotification =
      switch (breakingChange) {
      | Some(true) =>
        <MTypography
          variant=`Body2
          gutterBottom=true
          className=(Css.style([Css.color(Colors.error)]))>
          (
            {js|In preparation for our mainnet launch it has been necessary to make a breaking change. As a result your testnet ventures are no longer accessible. We will garuantee backwards compatibility following our public release on mainnet.|js}
            |> text
          )
        </MTypography>
      | _ => ReasonReact.null
      };
    let ventureList =
      switch (ventures) {
      | None => <Spinner text="loading index" />
      | Some([]) =>
        <MTypography variant=`Body2>
          (
            {js|You are not part of any Ventures yet. Create a new Venture, or join an existing Venture if you have an invite link from a Partner.|js}
            |> text
          )
        </MTypography>
      | Some(ventures) =>
        ReasonReact.array(
          Array.of_list(
            ventures
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
        breakingNotification
        ventureList
      </MaterialUi.List>
    </div>;
  },
};
