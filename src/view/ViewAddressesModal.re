open Belt;
include ViewCommon;
open PrimitiveTypes;

module ViewData = ViewModel.AddressesView;

type state = {expandedAddress: option(ViewData.addressInfo)};

type action =
  | ToggleAddress(ViewData.addressInfo);

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

let component = ReasonReact.reducerComponent("AddressesModal");

module Styles = {
  open Css;
  let chevron = rotate =>
    style([
      unsafe(
        "transition",
        "transform 150ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
      ),
      transform(rotateZ(deg(rotate ? 180 : 0))),
    ]);
  let grid =
    style([
      display(grid),
      unsafe("gridTemplateColumns", "[begin] 2fr 1fr 1fr min-content [end]"),
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
    ]);
  let detailsGrid =
    style([
      display(Css.grid),
      gridGap(px(Theme.space(3))),
      unsafe("gridTemplateColumns", "[begin] 1fr 1fr [end]"),
      padding4(
        ~right=px(Theme.space(3)),
        ~left=px(Theme.space(3)),
        ~top=px(Theme.space(4)),
        ~bottom=px(Theme.space(5)),
      ),
    ]);
};

let make = (~viewData: ViewData.t, _children) => {
  let renderExpandedInfo =
      (info: ViewData.addressInfo, details: ViewData.addressDetails) =>
    <div className=Styles.detailsGrid>
      <div>
        <MTypography gutterBottom=true variant=`Title>
          ("Custodians" |> text)
        </MTypography>
        <MTypography gutterBottom=true variant=`Body2>
          (
            "This is a "
            ++ string_of_int(details.nCoSigners)
            ++ "-of-"
            ++ string_of_int(details.nCustodians)
            ++ " address with the following custodians:"
            |> text
          )
        </MTypography>
        <MaterialUi.List>
          (
            Array.map(details.custodians |> Set.toArray, (partnerId: UserId.t) =>
              <Partner partnerId ex=(partnerId |> details.isPartner |> (!)) />
            )
            |> ReasonReact.array
          )
        </MaterialUi.List>
      </div>
      <div>
        <MTypography gutterBottom=true variant=`Title>
          ("OVERVIEW" |> text)
        </MTypography>
        <MTypography gutterBottom=true variant=`Body2>
          ("ADDRESS BALANCE: " ++ (info.balance |> BTC.format) |> text)
        </MTypography>
        <MaterialUi.List>
          (
            List.concat(details.unspentIncome, details.spentIncome)
            |. List.mapWithIndex((iter, tx: ViewData.income) => {
                 let (txType, primary) =
                   switch (tx.status) {
                   | _ => (Transaction.Income, "income")
                   };
                 <Transaction
                   key=(iter |> string_of_int)
                   txType
                   primary
                   amount=tx.amount
                   date=tx.date
                 />;
               })
            |> Utils.intersperse(key => <MDivider key />)
            |> List.toArray
            |> ReasonReact.array
          )
        </MaterialUi.List>
      </div>
    </div>;

  {
    ...component,
    initialState: () => {expandedAddress: None},
    reducer: (action, {expandedAddress}) =>
      switch (action) {
      | ToggleAddress(address) =>
        ReasonReact.Update({
          expandedAddress:
            expandedAddress == Some(address) ? None : Some(address),
        })
      },
    render: ({send, state}) => {
      let infos =
        viewData.infos
        |. List.keepMapU((. info: ViewData.addressInfo) =>
             if (info.addressType != WalletInfoCollector.Change
                 || info.balance
                 |> BTC.gt(BTC.zero)) {
               let details = viewData.addressDetails(info);
               let expand = state.expandedAddress == Some(info);
               [|
                 <MTypography className=Styles.summary variant=`Body2>
                   (info.address |> text)
                 </MTypography>,
                 <MTypography className=Styles.summary variant=`Body2>
                   (info.addressType |> addressTypeToString |> text)
                 </MTypography>,
                 <MTypography className=Styles.summary variant=`Body2>
                   (statusToString(info.addressStatus) |> text)
                 </MTypography>,
                 <MaterialUi.IconButton
                   className=(Styles.chevron(expand))
                   onClick=(_e => send(ToggleAddress(info)))>
                   Icons.chevronDown
                 </MaterialUi.IconButton>,
                 <MaterialUi.Collapse className=Styles.details in_=expand>
                   (renderExpandedInfo(info, details))
                 </MaterialUi.Collapse>,
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
                  ("ADDRESS TYPE" |> text)
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
