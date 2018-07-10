open Belt;
include ViewCommon;
open PrimitiveTypes;

module ViewData = ViewModel.AddressesView;

type state = {expandedAddress: option(ViewData.addressInfo)};

type action =
  | ToggleAddress(ViewData.addressInfo);

type status =
  | None
  | Accessible
  | AtRisk
  | TemporarilyInaccessible
  | PermanentlyInaccessible
  | PartiallyUnlocked
  | Unlocked
  | OldAddress;

let statusToColor =
  fun
  | None
  | Accessible => Colors.success
  | AtRisk
  | Unlocked
  | OldAddress => Colors.warning
  | PartiallyUnlocked
  | TemporarilyInaccessible
  | PermanentlyInaccessible => Colors.error;

let statusToString =
  fun
  | None => ""
  | Accessible => "Accessible"
  | AtRisk => "At Risk"
  | Unlocked => "Unlocked"
  | OldAddress => "Old Address"
  | PartiallyUnlocked => "Partially Unlocked"
  | TemporarilyInaccessible => "Temporarily Inaccessible"
  | PermanentlyInaccessible => "Permanently Inaccessible";

let statusToLabel = (~className="", status) =>
  status == None ?
    ReasonReact.null :
    <MTypography
      className=(
        Css.(style([color(statusToColor(status))])) ++ " " ++ className
      )
      variant=`Body2>
      (status |> statusToString |> String.uppercase |> text)
    </MTypography>;

let calcAddressStatus =
    (status: ViewData.addressStatus, unlocked: list(bool)) =>
  switch (status, unlocked) {
  | (Accessible, _) => Accessible
  | (AtRisk, _) => AtRisk
  | (OutdatedCustodians, _) => OldAddress
  | (TemporarilyInaccessible, unlocked) when unlocked |. List.some(b => b) =>
    PartiallyUnlocked
  | (TemporarilyInaccessible, _) => TemporarilyInaccessible
  | (Inaccessible, _) => PermanentlyInaccessible
  };

let calcTransactionStatus = (status: ViewData.addressStatus, unlocked: bool) =>
  switch (status, unlocked) {
  | (TemporarilyInaccessible, true) => Unlocked
  | (TemporarilyInaccessible, false) => TemporarilyInaccessible
  | (_, _) => None
  };

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
      unsafe("gridTemplateColumns", "[begin] 1fr 1fr 1fr min-content [end]"),
    ]);
  let header = warning =>
    style([
      borderBottom(px(1), `solid, hex("979797")),
      padding2(~v=px(Theme.space(2)), ~h=px(Theme.space(3))),
      position(sticky),
      zIndex(1),
      top(px(warning ? Theme.space(4) : 0)),
      backgroundColor(Colors.white),
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
  let changeAddress =
    style([color(rgba(0, 0, 0, 0.5)), textTransform(uppercase)]);
};

let make = (~viewData: ViewData.t, _children) => {
  let renderTx =
      (addressStatus, txList: list(ViewData.income), txTypeString: string) =>
    List.mapWithIndex(
      txList,
      (iter, tx: ViewData.income) => {
        let primary =
          switch (tx.status) {
          | Unconfirmed => "unconfirmed " ++ txTypeString
          | Confirmed => txTypeString
          };
        let label =
          calcTransactionStatus(addressStatus, tx.unlocked)
          |> statusToLabel(~className=Css.(style([Css.float(`right)])));
        <Transaction
          key=(iter |> string_of_int)
          txType=Income
          primary
          amount=tx.amount
          date=tx.date
          label
        />;
      },
    );
  let renderExpandedInfo = (details: ViewData.addressDetails) =>
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
        <MaterialUi.List>
          (
            List.concat(
              renderTx(
                details.addressStatus,
                details.unspentIncome,
                "income",
              ),
              renderTx(
                details.addressStatus,
                details.spentIncome,
                "income - transferred",
              ),
            )
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
                   (
                     switch (info.addressType) {
                     | WalletInfoCollector.Income(_) => info.address |> text
                     | WalletInfoCollector.Change =>
                       <span className=Styles.changeAddress>
                         ("(hidden change address)" |> text)
                       </span>
                     }
                   )
                 </MTypography>,
                 <MTypography className=Styles.summary variant=`Body2>
                   ((info.balance |> BTC.format) ++ " BTC" |> text)
                 </MTypography>,
                 calcAddressStatus(
                   info.addressStatus,
                   List.map(details.unspentIncome, i => i.unlocked),
                 )
                 |> statusToLabel(~className=Styles.summary),
                 <MaterialUi.IconButton
                   className=(Styles.chevron(expand))
                   onClick=(_e => send(ToggleAddress(info)))>
                   Icons.chevronDown
                 </MaterialUi.IconButton>,
                 <MaterialUi.Collapse className=Styles.details in_=expand>
                   (renderExpandedInfo(details))
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
          <WarningBanner key="warning">
            ...(viewData.ventureId |> WarningsText.atRiskFunds)
          </WarningBanner> :
          ReasonReact.null;
      let className = Styles.header(viewData.atRiskWarning);
      <Grid
        title1=("Wallet Address History" |> text)
        area3={
          <div className=ScrollList.containerStyles>
            <ScrollList>
              warning
              <div className=Styles.grid>
                <MTypography className variant=`Body2>
                  ("WALLET ADDRESS" |> text)
                </MTypography>
                <MTypography className variant=`Body2>
                  ("ADDRESS BALANCE" |> text)
                </MTypography>
                <MTypography className variant=`Body2>
                  ("STATUS" |> text)
                </MTypography>
                <span className />
                infos
              </div>
            </ScrollList>
          </div>
        }
      />;
    },
  };
};
