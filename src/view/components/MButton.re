let component = ReasonReact.statelessComponent("MButton");

let make = (~color=?, ~onClick=?, children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classes=[
          {
            name: "button",
            styles:
              ReactDOMRe.Style.make(
                ~borderRadius="25px",
                ~border="solid 1.5px #000000",
                ~margin="1.5px",
                ~paddingLeft="25px",
                ~paddingRight="25px",
                (),
              ),
          },
        ]
        render=(
          classes =>
            <Button className=classes##button ?color ?onClick>
              children
            </Button>
        )
      />
    ),
};
