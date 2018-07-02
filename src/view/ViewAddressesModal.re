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

let make = (~viewData: ViewData.t, _children) => {
  Js.log2("viewdata", viewData);
  {
    ...component,
    render: _ => {
      let infos =
        viewData
        |. List.mapU((. info: ViewData.addressInfo) =>
             <li>
               (
                 info.address
                 ++ " "
                 ++ statusToString(info.addressStatus)
                 |> text
               )
             </li>
           )
        |> List.toArray
        |> ReasonReact.array;
      <div> <ul> infos </ul> </div>;
    },
  };
};
