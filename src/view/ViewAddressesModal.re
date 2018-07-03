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
  | WalletInfoCollector.Income => "Income"
  | WalletInfoCollector.Change => "Change";

let component = ReasonReact.statelessComponent("AddressesModal");

let renderExpandedInfo = (info: ViewData.addressDetails) =>
  <div>
    (
      (
        (
          string_of_int(info.nCoSigners)
          ++ "-"
          ++ string_of_int(info.nCustodians)
          ++ "; ["
          |. Set.reduceU(info.custodians, _, (. res, c) =>
               res ++ UserId.toString(c) ++ ", "
             )
        )
        ++ "] ["
        |. List.reduceU(
             List.concat(info.currentUtxos, info.spentInputs),
             _,
             (. res, input: Network.txInput) =>
             res ++ (input.value |> BTC.format) ++ ", "
           )
      )
      ++ "] "
      ++ addressTypeToString(info.addressType)
      |> text
    )
  </div>;

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
