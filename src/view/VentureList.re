open PrimitiveTypes;

let component = ReasonReact.statelessComponent("VentureList");

let make = (~selected=?, ~index, _children) => {
  ...component,
  render: _self => {
    let ventureList =
      ReasonReact.arrayToElement(
        Array.of_list(
          index
          |> List.map(
               Venture.Index.(
                 ({name, id}) =>
                   MaterialUi.(
                     <WithStyles
                       key=(id |> VentureId.toString)
                       classes=[
                         {
                           name: "linkSelected",
                           styles:
                             ReactDOMRe.Style.make(
                               ~display="block",
                               ~width="100%",
                               ~color="#02a2b4",
                               (),
                             ),
                         },
                         {
                           name: "link",
                           styles:
                             ReactDOMRe.Style.make(
                               ~display="block",
                               ~width="100%",
                               ~color="inherit",
                               (),
                             ),
                         },
                       ]
                       render={
                                let ids = id |> VentureId.toString;
                                classes =>
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
                                              classes##linkSelected :
                                              classes##link
                                          )
                                          route=(Venture(id))>
                                          (name |> ReasonReact.stringToElement)
                                        </Link>
                                      }
                                    />
                                  </ListItem>;
                              }
                     />
                   )
               ),
             ),
        ),
      );
    let _status = ReasonReact.stringToElement("ventures:");
    let title =
      MaterialUi.(
        <WithStyles
          key="ventures-title"
          classes=[
            {
              name: "title",
              styles: ReactDOMRe.Style.make(~padding="7px 20px", ()),
            },
          ]
          render=(
            classes =>
              <TitleBar>
                <Typography className=classes##title variant=`Title>
                  (ReasonReact.stringToElement("My Ventures"))
                </Typography>
              </TitleBar>
          )
        />
      );
    <div>
      title
      <MaterialUi.List dense=true> ventureList </MaterialUi.List>
    </div>;
  },
};
