open Belt;
include ViewCommon;
open PrimitiveTypes;

module ViewData = ViewModel.AddressesView;

let statusToString =
  fun
  | WalletInfoCollector.Accessible => "Accessible"
  | WalletInfoCollector.AtRisk => "AtRisk"
  | WalletInfoCollector.OutdatedCustodians => "OutdatedCustodians"
  | WalletInfoCollector.TemporarilyInaccessible => "TemporarilyInaccessible"
  | WalletInfoCollector.Inaccessible => "Inaccessible";
let addressTypeToString =
  fun
  | WalletInfoCollector.Income(id) =>
    "Income (exposed by - " ++ UserId.toString(id) ++ ")"
  | WalletInfoCollector.Change => "Change";

let component = ReasonReact.statelessComponent("AddressesModal");

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

let make = (~viewData: ViewData.t, _children) => {
  let renderExpandedInfo = (info: ViewData.addressDetails) =>
    MaterialUi.(
      <Collapse className=Styles.details in_=true>
        <div className=Styles.detailsGrid>
          <div>
            <MTypography gutterBottom=true variant=`Title>
              ("Custodians" |> text)
            </MTypography>
            <MTypography gutterBottom=true variant=`Body1>
              (
                "This is a "
                ++ string_of_int(info.nCoSigners)
                ++ "-of-"
                ++ string_of_int(info.nCustodians)
                ++ " address with the following custodians:"
                |> text
              )
            </MTypography>
            <List>
              (
                Array.map(
                  info.custodians |> Set.toArray,
                  (partnerId: UserId.t) => {
                    let status =
                      partnerId |> info.isPartner ?
                        None : Some(" - Ex-Partner" |> text);
                    <Partner partnerId ?status />;
                  },
                )
                |> ReasonReact.array
              )
            </List>
          </div>
          <div>
            <MTypography gutterBottom=true variant=`Title>
              ("OVERVIEW" |> text)
            </MTypography>
            <MTypography gutterBottom=true variant=`Body1>
              ("ADDRESS BALANCE: TODO" |> text)
            </MTypography>
            (
              Array.map(
                Belt.List.concat(info.currentUtxos, info.spentInputs)
                |> Belt.List.toArray,
                (input: Network.txInput) =>
                input.value |> BTC.format |> text
              )
              |> ReasonReact.array
            )
          </div>
        </div>
      </Collapse>
    );
  {
    ...component,
    render: _ => {
      let infos =
        viewData.infos
        |. List.keepMapU((. info: ViewData.addressInfo) =>
             if (info.addressType != WalletInfoCollector.Change
                 || info.balance
                 |> BTC.gt(BTC.zero)) {
               let expandedInfo = viewData.addressDetails(info);
               [|
                 <MTypography className=Styles.summary variant=`Body2>
                   (info.address |> text)
                 </MTypography>,
                 <MTypography className=Styles.summary variant=`Body2>
                   (statusToString(info.addressStatus) |> text)
                 </MTypography>,
                 <MaterialUi.IconButton>
                   Icons.chevronDown
                 </MaterialUi.IconButton>,
                 expandedInfo |> renderExpandedInfo,
               |]
               |> ReasonReact.array
               |. Some;
             } else {
               None;
             }
           )
        |> List.toArray
        |> ReasonReact.array;
      <Grid
        title1=("Wallet Address History" |> text)
        area3={
          <div className=ScrollList.containerStyles>
            <ScrollList>
              <div className=Styles.grid>
                <MTypography className=Styles.header variant=`Body2>
                  ("WALLET ADDRESS" |> text)
                </MTypography>
                <MTypography className=Styles.header variant=`Body2>
                  ("STATUS" |> text)
                </MTypography>
                <span className=Styles.header />
                infos
              </div>
            </ScrollList>
          </div>
        }
      />;
    },
  };
};
