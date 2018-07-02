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

let renderExpandedInfo = expandedInfo => <div> ("expanded" |> text) </div>;
let make = (~viewData: ViewData.t, _children) => {
  ...component,
  render: _ => {
    let infos =
      viewData.infos
      |. List.mapU((. info: ViewData.addressInfo) => {
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
           </li>;
         })
      |> List.toArray
      |> ReasonReact.array;
    <div> <ul> infos </ul> </div>;
  },
};
