open Venture;

open PrimitiveTypes;

let text = ReasonReact.stringToElement;

type state = {
  venture: Venture.t(ViewModel.t),
  viewModel: ViewModel.t,
  prospectId: string,
  balance: Venture.Wallet.balance,
  syncWorker: ref(SyncWorker.t),
  incomeWorker: ref(IncomeWorker.t),
};

type action =
  | SyncWorkerMessage(SyncWorker.Message.receive)
  | IncomeWorkerMessage(IncomeWorker.Message.receive)
  | ChangeNewPartnerId(string)
  | UpdateVenture(Venture.t(ViewModel.t))
  | ProposePartner
  | EndorsePartner(ProcessId.t)
  | RemovePartner(UserId.t)
  | EndorsePartnerRemoval(ProcessId.t)
  | GetIncomeAddress
  | ProposePayout(list((string, BTC.t)))
  | EndorsePayout(ProcessId.t);

let changeNewPartnerId = event =>
  ChangeNewPartnerId(
    ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
  );

let component = ReasonReact.reducerComponent("SelectedVenture");

let make = (~venture as initialVenture, ~session: Session.Data.t, _children) => {
  ...component,
  initialState: () => {
    balance: initialVenture |> Venture.Wallet.balance,
    venture: initialVenture,
    viewModel: Venture.getListenerState(initialVenture),
    prospectId: "",
    syncWorker: ref(SyncWorker.make(~onMessage=Js.log)),
    incomeWorker: ref(IncomeWorker.make(~onMessage=Js.log)),
  },
  subscriptions: ({send, state}) => [
    Sub(
      () => {
        SyncWorker.terminate(state.syncWorker^);
        let worker =
          SyncWorker.make(~onMessage=message =>
            send(SyncWorkerMessage(message))
          );
        Js.Promise.(
          Venture.getPartnerHistoryUrls(initialVenture)
          |> then_(urls =>
               SyncWorker.Message.RegularlyFetch(
                 urls,
                 Venture.getSummary(initialVenture),
               )
               |> SyncWorker.postMessage(worker)
               |> resolve
             )
          |> ignore
        );
        state.syncWorker := worker;
        worker;
      },
      SyncWorker.terminate,
    ),
    Sub(
      () => {
        IncomeWorker.terminate(state.incomeWorker^);
        let worker =
          IncomeWorker.make(~onMessage=message =>
            send(IncomeWorkerMessage(message))
          );
        IncomeWorker.Message.MonitorAddresses(
          initialVenture |> Venture.Wallet.getExposedAddresses,
          initialVenture |> Venture.Wallet.getKnownTransactionIds,
        )
        |> IncomeWorker.postMessage(worker)
        |> ignore;
        state.incomeWorker := worker;
        worker;
      },
      IncomeWorker.terminate,
    ),
  ],
  reducer: (action, state) =>
    switch (action) {
    | SyncWorkerMessage(Fetched(eventLogs)) =>
      ReasonReact.SideEffects(
        (
          ({send, state}) =>
            Js.Promise.(
              Cmd.SynchronizeLogs.(
                state.venture
                |> exec(eventLogs)
                |> then_(
                     fun
                     | Ok(venture) =>
                       send(UpdateVenture(venture)) |> resolve
                     | Error(venture, {event}, result) => {
                         Js.log("An error occured while synchronizing");
                         Js.log("Adding event: ");
                         Js.log(Event.encode(event));
                         Js.log2(
                           "failed because: ",
                           Venture.Validation.resultToString(result),
                         );
                         send(UpdateVenture(venture)) |> resolve;
                       },
                   )
                |> ignore
              )
            )
        ),
      )
    | IncomeWorkerMessage(msg) =>
      switch (msg) {
      | NewTransactionsDetected(txs) =>
        ReasonReact.SideEffects(
          (
            ({send, state}) =>
              Js.Promise.(
                Cmd.SynchronizeWallet.(
                  state.venture
                  |> exec(txs)
                  |> then_(
                       fun
                       | Ok(venture) =>
                         send(UpdateVenture(venture)) |> resolve,
                     )
                  |> ignore
                )
              )
          ),
        )
      }
    | ChangeNewPartnerId(text) =>
      ReasonReact.Update({...state, prospectId: text})
    | ProposePartner =>
      switch (String.trim(state.prospectId)) {
      | "" => ReasonReact.NoUpdate
      | prospectId =>
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                Cmd.ProposePartner.(
                  state.venture
                  |> exec(~prospectId=prospectId |> UserId.fromString)
                  |> then_(result =>
                       (
                         switch (result) {
                         | Ok(venture) => send(UpdateVenture(venture))
                         | PartnerAlreadyExists =>
                           Js.log("PartnerAlreadyExists")
                         | NoUserInfo => Js.log("NoUserInfo")
                         }
                       )
                       |> resolve
                     )
                  |> ignore
                )
              )
          ),
        )
      }
    | EndorsePartner(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePartner.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | RemovePartner(partnerId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ProposePartnerRemoval.(
                state.venture
                |> exec(~partnerId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       | PartnerDoesNotExist => Js.log("PartnerDoesNotExist")
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | EndorsePartnerRemoval(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePartnerRemoval.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | UpdateVenture(venture) =>
      Js.Promise.(
        Venture.getPartnerHistoryUrls(venture)
        |> then_(urls =>
             SyncWorker.Message.RegularlyFetch(
               urls,
               Venture.getSummary(venture),
             )
             |> SyncWorker.postMessage(state.syncWorker^)
             |> resolve
           )
        |> ignore
      );
      IncomeWorker.Message.MonitorAddresses(
        venture |> Venture.Wallet.getExposedAddresses,
        venture |> Venture.Wallet.getKnownTransactionIds,
      )
      |> IncomeWorker.postMessage(state.incomeWorker^)
      |> ignore;
      ReasonReact.Update({
        ...state,
        venture,
        viewModel: venture |> Venture.getListenerState,
        balance: venture |> Venture.Wallet.balance,
      });
    | GetIncomeAddress =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ExposeIncomeAddress.(
                state.venture
                |> exec(~accountIdx=WalletTypes.AccountIndex.default)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(_, venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | ProposePayout(destinations) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.ProposePayout.(
                state.venture
                |> exec(
                     ~accountIdx=WalletTypes.AccountIndex.default,
                     ~destinations,
                     ~fee=BTC.fromSatoshis(5L),
                   )
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    | EndorsePayout(processId) =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Cmd.EndorsePayout.(
                state.venture
                |> exec(~processId)
                |> then_(result =>
                     (
                       switch (result) {
                       | Ok(venture) => send(UpdateVenture(venture))
                       }
                     )
                     |> resolve
                   )
                |> ignore
              )
            )
        ),
      )
    },
  render: ({send, state}) => {
    let partners =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.partners(state.viewModel)
          |> List.map((m: ViewModel.partner) =>
               <li key=(m.userId |> UserId.toString)>
                 <div>
                   (text(m.userId |> UserId.toString))
                   (
                     if (m.userId |> UserId.neq(session.userId)) {
                       <button onClick=(_e => send(RemovePartner(m.userId)))>
                         (text("Propose Removal"))
                       </button>;
                     } else {
                       ReasonReact.nullElement;
                     }
                   )
                 </div>
               </li>
             ),
        ),
      );
    let prospects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.prospects(state.viewModel)
          |> List.map((prospect: ViewModel.prospect) =>
               <li key=(prospect.userId |> UserId.toString)>
                 (
                   text(
                     "'"
                     ++ (prospect.userId |> UserId.toString)
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          prospect.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   if (prospect.endorsedBy |> List.mem(session.userId) == false) {
                     <button
                       onClick=(
                         _e => send(EndorsePartner(prospect.processId))
                       )>
                       (text("Endorse Partner"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
               </li>
             ),
        ),
      );
    let removalProspects =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.removalProspects(state.viewModel)
          |> List.map((prospect: ViewModel.prospect) =>
               <li key=(prospect.userId |> UserId.toString)>
                 (
                   text(
                     "'"
                     ++ (prospect.userId |> UserId.toString)
                     ++ "' endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          prospect.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   if (prospect.endorsedBy |> List.mem(session.userId) == false) {
                     <button
                       onClick=(
                         _e =>
                           send(EndorsePartnerRemoval(prospect.processId))
                       )>
                       (text("Endorse Removal"))
                     </button>;
                   } else {
                     ReasonReact.nullElement;
                   }
                 )
               </li>
             ),
        ),
      );
    let addresses =
      ReasonReact.arrayToElement(
        Array.of_list(
          ViewModel.incomeAddresses(state.viewModel)
          |> List.map(address => <li key=address> (text(address)) </li>),
        ),
      );
    let payouts =
      ReasonReact.arrayToElement(
        Array.of_list(
          state.viewModel
          |> ViewModel.payouts
          |> List.map((payout: ViewModel.payout) =>
               <li key=(payout.processId |> ProcessId.toString)>
                 (
                   text(
                     "'"
                     ++ (payout.processId |> ProcessId.toString)
                     ++ "' status: "
                     ++ (
                       switch (payout.status) {
                       | PayoutPending => "pending"
                       | PayoutCompleted(id) =>
                         "completed (txId: " ++ id ++ ")"
                       | PayoutFailed(reason) =>
                         "failed (error: '" ++ reason ++ "')"
                       }
                     )
                     ++ " endorsed by: "
                     ++ List.fold_left(
                          (state, partnerId) => state ++ partnerId ++ " ",
                          "",
                          payout.endorsedBy |> List.map(UserId.toString),
                        ),
                   )
                 )
                 (
                   switch (
                     payout.status,
                     payout.endorsedBy |> List.mem(session.userId),
                   ) {
                   | (PayoutPending, false) =>
                     <button
                       onClick=(_e => send(EndorsePayout(payout.processId)))>
                       (text("Endorse Payout"))
                     </button>
                   | _ => ReasonReact.nullElement
                   }
                 )
               </li>
             ),
        ),
      );
    <div>
      <div>
        <h2> (text(ViewModel.ventureName(state.viewModel))) </h2>
        (
          text(
            "Join Venture url: "
            ++ Location.origin
            ++ Router.Config.routeToUrl(
                 JoinVenture(initialVenture |> Venture.getId, session.userId),
               ),
          )
        )
        <h3> (text("Partners:")) </h3>
        <ul> partners </ul>
        <h4> (text("Prospects:")) </h4>
        <ul> prospects </ul>
        <h4> (text("To be removed:")) </h4>
        <ul> removalProspects </ul>
        <input
          placeholder="BlockstackId"
          value=state.prospectId
          onChange=(e => send(changeNewPartnerId(e)))
          autoFocus=Js.false_
        />
        <button onClick=(_e => send(ProposePartner))>
          (text("Propose Partner"))
        </button>
        <h3> (text("Wallet:")) </h3>
        <h4> (text("blance: ")) </h4>
        (
          text(
            "income: "
            ++ BTC.format(state.balance.income)
            ++ " spent: "
            ++ BTC.format(state.balance.spent)
            ++ " reserved: "
            ++ BTC.format(state.balance.reserved),
          )
        )
        <h4> (text("Income Addresses:")) </h4>
        <ul> addresses </ul>
        <button onClick=(_e => send(GetIncomeAddress))>
          (text("Get New Income Address"))
        </button>
        <Payout onSend=(destinations => send(ProposePayout(destinations))) />
        <h4> (text("Payouts:")) </h4>
        <ul> payouts </ul>
      </div>
    </div>;
  },
};
