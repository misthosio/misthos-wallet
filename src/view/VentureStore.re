type ventureState =
  | None
  | CreatingVenture
  | JoiningVenture
  | LoadingVenture
  | VentureLoaded(Venture.t(ViewModel.t));

type action =
  | UpdateIndex(Venture.Index.t)
  | UpdateVenture(ventureState)
  | CreateVenture(Session.Data.t, string)
  | SyncWorkerMessage(SyncWorker.Message.receive)
  | IncomeWorkerMessage(IncomeWorker.Message.receive);

type state = {
  index: Venture.Index.t,
  ventureState,
  syncWorker: ref(SyncWorker.t),
  incomeWorker: ref(IncomeWorker.t),
};

let loadVentureAndIndex =
    (send, session: Session.t, currentRoute, {ventureState}) => {
  switch (session) {
  | LoggedIn(_) =>
    Js.Promise.(
      Venture.Index.load()
      |> then_(index => send(UpdateIndex(index)) |> resolve)
      |> ignore
    )
  | _ => ()
  };
  switch (session, currentRoute: Router.Config.route, ventureState) {
  | (LoggedIn(sessionData), Venture(id), VentureLoaded(venture))
      when id != (venture |> Venture.getId) =>
    Js.Promise.(
      Venture.load(
        sessionData,
        ~ventureId=id,
        ~listenerState=ViewModel.make(),
        ~listener=ViewModel.apply,
      )
      |> then_(venture =>
           send(UpdateVenture(VentureLoaded(venture))) |> resolve
         )
      |> ignore
    );
    LoadingVenture;
  | (LoggedIn(_), Venture(id), VentureLoaded(venture))
      when id == (venture |> Venture.getId) => ventureState
  | (LoggedIn(sessionData), Venture(id), _) =>
    Js.Promise.(
      Venture.load(
        sessionData,
        ~ventureId=id,
        ~listenerState=ViewModel.make(),
        ~listener=ViewModel.apply,
      )
      |> then_(venture =>
           send(UpdateVenture(VentureLoaded(venture))) |> resolve
         )
      |> ignore
    );
    LoadingVenture;
  | (LoggedIn(sessionData), JoinVenture(ventureId, userId), _) =>
    Js.Promise.(
      Venture.join(
        sessionData,
        ~userId,
        ~ventureId,
        ~listenerState=ViewModel.make(),
        ~listener=ViewModel.apply,
      )
      |> then_(((index, venture)) => {
           send(UpdateIndex(index));
           send(UpdateVenture(VentureLoaded(venture)));
           ReasonReact.Router.push(
             Router.Config.routeToUrl(
               Router.Config.Venture(venture |> Venture.getId),
             ),
           )
           |> resolve;
         })
      |> ignore
    );
    JoiningVenture;
  | _ => None
  };
};

let component = ReasonReact.reducerComponent("VentureStore");

let make = (~currentRoute, ~session: Session.t, children) => {
  ...component,
  initialState: () => {
    index: [],
    ventureState: None,
    syncWorker: ref(SyncWorker.make(~onMessage=Js.log)),
    incomeWorker: ref(IncomeWorker.make(~onMessage=Js.log)),
  },
  didMount: ({send, state}) =>
    ReasonReact.Update({
      ...state,
      ventureState: loadVentureAndIndex(send, session, currentRoute, state),
    }),
  willReceiveProps: ({send, state}) => {
    ...state,
    ventureState: loadVentureAndIndex(send, session, currentRoute, state),
  },
  subscriptions: ({send, state}) => [
    Sub(
      () => {
        SyncWorker.terminate(state.syncWorker^);
        let worker =
          SyncWorker.make(~onMessage=message =>
            send(SyncWorkerMessage(message))
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
              Venture.Cmd.SynchronizeLogs.(
                switch (state.ventureState) {
                | VentureLoaded(venture) =>
                  venture
                  |> exec(eventLogs)
                  |> then_(
                       fun
                       | Ok(venture) =>
                         send(UpdateVenture(VentureLoaded(venture)))
                         |> resolve
                       | Error(venture, {event}, result) => {
                           Js.log("An error occured while synchronizing");
                           Js.log("Adding event: ");
                           Js.log(Event.encode(event));
                           Js.log2(
                             "failed because: ",
                             Venture.Validation.resultToString(result),
                           );
                           send(UpdateVenture(VentureLoaded(venture)))
                           |> resolve;
                         },
                     )
                  |> ignore
                | _ =>
                  SyncWorker.Message.Wait
                  |> SyncWorker.postMessage(state.syncWorker^)
                }
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
                Venture.Cmd.SynchronizeWallet.(
                  switch (state.ventureState) {
                  | VentureLoaded(venture) =>
                    venture
                    |> exec(txs)
                    |> then_(
                         fun
                         | Ok(venture) =>
                           send(UpdateVenture(VentureLoaded(venture)))
                           |> resolve,
                       )
                    |> ignore
                  | _ =>
                    IncomeWorker.Message.Wait
                    |> IncomeWorker.postMessage(state.incomeWorker^)
                  }
                )
              )
          ),
        )
      }
    | UpdateIndex(index) => ReasonReact.Update({...state, index})
    | UpdateVenture(ventureState) =>
      switch (ventureState) {
      | VentureLoaded(venture) =>
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
      | _ =>
        SyncWorker.Message.Wait |> SyncWorker.postMessage(state.syncWorker^);
        IncomeWorker.Message.Wait
        |> IncomeWorker.postMessage(state.incomeWorker^);
      };
      ReasonReact.Update({...state, ventureState});
    | CreateVenture(session, name) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, ventureState: CreatingVenture},
        (
          ({send}) =>
            Js.Promise.(
              Venture.Cmd.Create.exec(
                session,
                ~name,
                ~listenerState=ViewModel.make(),
                ~listener=ViewModel.apply,
              )
              |> then_(((newIndex, venture)) => {
                   send(UpdateVenture(VentureLoaded(venture)));
                   send(UpdateIndex(newIndex));
                   ReasonReact.Router.push(
                     Router.Config.routeToUrl(
                       Router.Config.Venture(venture |> Venture.getId),
                     ),
                   )
                   |> resolve;
                 })
              |> ignore
            )
        ),
      )
    },
  render: ({state: {index, ventureState}, send}) =>
    children(~index, ~selectedVenture=ventureState, ~updateVentureStore=send),
};
