include ViewCommon;

module ViewData = ViewModel.SelectedVentureView;

let component = ReasonReact.statelessComponent("AddressListItem");

module Styles = {
  open Css;
  let grid =
    style([
      display(grid),
      unsafe("gridTemplateColumns", "[begin] 5fr 1fr min-content [end]"),
    ]);
  let header =
    style([
      borderBottom(px(1), `solid, hex("979797")),
      padding2(~v=px(Theme.space(2)), ~h=px(Theme.space(3))),
    ]);
  let summary =
    style([padding2(~v=px(Theme.space(2)), ~h=px(Theme.space(3)))]);
  let details =
    style([
      unsafe("gridColumn", "begin / end"),
      borderBottom(px(1), `solid, hex("979797")),
      paddingBottom(px(Theme.space(5))),
    ]);
  let detailsGrid =
    style([
      display(Css.grid),
      gridGap(px(Theme.space(3))),
      unsafe("gridTemplateColumns", "[begin] 1fr 1fr [end]"),
      padding2(~v=px(0), ~h=px(Theme.space(3))),
    ]);
};

let make = _children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <div className=Styles.grid>
        <MTypography className=Styles.header variant=`Body2>
          ("WALLET ADDRESS" |> text)
        </MTypography>
        <MTypography className=Styles.header variant=`Body2>
          ("STATUS" |> text)
        </MTypography>
        <span className=Styles.header />
        <MTypography className=Styles.summary variant=`Body2>
          ("2MvVjSAA2JDqdQQ3uaTGa9P1jZddhVFsnxa" |> text)
        </MTypography>
        <MTypography className=Styles.summary variant=`Body2>
          ("ACCESSIBLE" |> text)
        </MTypography>
        <IconButton> Icons.chevronDown </IconButton>
        <Collapse className=Styles.details in_=true>
          <div className=Styles.detailsGrid>
            <Typography>
              (
                {|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse malesuada lacus ex,
            sit amet blandit leo lobortis eget.|}
                |> text
              )
            </Typography>
            <Typography>
              (
                {|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse malesuada lacus ex,
            sit amet blandit leo lobortis eget.|}
                |> text
              )
            </Typography>
          </div>
        </Collapse>
        <MTypography className=Styles.summary variant=`Body2>
          ("2MvVjSAA2JDqdQQ3uaTGa9P1jZddhVFsnxa" |> text)
        </MTypography>
        <MTypography className=Styles.summary variant=`Body2>
          ("ACCESSIBLE" |> text)
        </MTypography>
        <IconButton> Icons.chevronDown </IconButton>
        <Collapse className=Styles.details in_=true>
          <div className=Styles.detailsGrid>
            <Typography>
              (
                {|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse malesuada lacus ex,
            sit amet blandit leo lobortis eget.|}
                |> text
              )
            </Typography>
            <Typography>
              (
                {|Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse malesuada lacus ex,
            sit amet blandit leo lobortis eget.|}
                |> text
              )
            </Typography>
          </div>
        </Collapse>
      </div>
    ),
};
