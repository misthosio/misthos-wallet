open Belt;
include ViewCommon;

module ViewData = ViewModel.AddressesView;

let statusToString =
  fun
  | WalletInfoCollector.Accessible => "Accessible"
  | WalletInfoCollector.AtRisk => "AtRisk"
  | WalletInfoCollector.OutdatedCustodians => "OutdatedCustodians"
  | WalletInfoCollector.TemporarilyInaccessible => "TemporarilyInaccessible"
  | WalletInfoCollector.Inaccessible => "Inaccessible";

let component = ReasonReact.statelessComponent("AddressesModal");

let renderExpandedInfo = _expandedInfo => <div> ("expanded" |> text) </div>;

let make = (~viewData: ViewData.t, _children) => {
  ...component,
  render: _ => {
    let infos =
      viewData.infos
      |. List.keepMapU((. info: ViewData.addressInfo) =>
           if (info.addressType == WalletInfoCollector.Income
               || info.balance
               |> BTC.gt(BTC.zero)) {
             let expandedInfo = viewData.addressDetails(info);
             <li>
               <div>
                 (
                   info.address
                   ++ " "
                   ++ (info.balance |> BTC.format)
                   ++ " "
                   ++ statusToString(info.addressStatus)
                   |> text
                 )
               </div>
               (expandedInfo |> renderExpandedInfo)
             </li>
             |. Some;
           } else {
             None;
           }
         )
      |> List.toArray
      |> ReasonReact.array;
    <div> <ul> infos </ul> </div>;
  },
};
