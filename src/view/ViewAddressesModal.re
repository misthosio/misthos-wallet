open Belt;
include ViewCommon;
open PrimitiveTypes;

module ViewData = ViewModel.AddressesView;

type state = {expandedAddress: option(ViewData.addressInfo)};

type action =
  | ToggleAddress(ViewData.addressInfo);

type status =
  | Accessible
  | AtRisk
  | OutdatedCustodians
  | TemporarilyInaccessible
  | PermanentlyInaccessible
  | PartiallyUnlocked
  | Unlocked
  | OldAddress;

let statusToColor =
  fun
  | Accessible => Colors.success
  | AtRisk
  | Unlocked
  | OldAddress => Colors.warning
  | OutdatedCustodians
  | PartiallyUnlocked
  | TemporarilyInaccessible
  | PermanentlyInaccessible => Colors.error;

let statusToString =
  fun
  | Accessible => "Accessible"
  | AtRisk => "At Risk"
  | Unlocked => "Unlocked"
  | OldAddress => "Old Address"
  | OutdatedCustodians => "Outdated Custodians"
  | PartiallyUnlocked => "Partially Unlocked"
  | TemporarilyInaccessible => "Temporarily Inaccessible"
  | PermanentlyInaccessible => "Permanently Inaccessible";

let statusToLable = (~className="", status) =>
  <MTypography
    className=(
      Css.(style([color(statusToColor(status))])) ++ " " ++ className
    )
    variant=`Body2>
    (status |> statusToString |> String.uppercase |> text)
  </MTypography>;

let calcStatus =
    (status: ViewData.addressStatus, balance, unlocked: list(bool)) =>
  /* List.some(unlocked, (== true)) */
  switch (status, balance, unlocked) {
  /* | (_, _, true) => Unlocked */
  | (Accessible, _, _) => Accessible
  | (AtRisk, _, _) => AtRisk
  | (OutdatedCustodians, _, _) => OutdatedCustodians
  | (TemporarilyInaccessible, _, _) => TemporarilyInaccessible
  | (Inaccessible, _, _) => PermanentlyInaccessible
  };

let addressTypeToString =
  fun
  | WalletInfoCollector.Income(_) => "Income"
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
            |. List.concat(
                 details.addressType
                 |> (
                   fun
                   | WalletInfoCollector.Income(id) => [
                       <MaterialUi.ListItem
                         disableGutters=true
                         divider=true
                         classes=[Divider(Transaction.Styles.divider)]>
                         <MaterialUi.ListItemText
                           classes=[Root(Transaction.Styles.root)]
                           primary={
                             <MTypography variant=`Body2>
                               (
                                 "address exposed by "
                                 ++ UserId.toString(id)
                                 |> String.uppercase
                                 |> text
                               )
                             </MTypography>
                           }
                         />
                       </MaterialUi.ListItem>,
                     ]
                   | _ => []
                 ),
               )
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
                   (
                     info.addressType
                     |> addressTypeToString
                     |> String.uppercase
                     |> text
                   )
                 </MTypography>,
                 calcStatus(
                   info.addressStatus,
                   info.balance,
                   List.map(details.unspentIncome, i => i.unlocked),
                 )
                 |> statusToLable(~className=Styles.summary),
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
      let warning =
        viewData.atRiskWarning ?
          Some(WarningsText.atRiskFunds(viewData.ventureId)) : None;
      <Grid
        ?warning
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
