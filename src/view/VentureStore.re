open PrimitiveTypes;

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
  | IncomeWorkerMessage(IncomeWorkerMessage.receive)
  | PersistWorkerMessage(PersistWorkerMessage.receive);

type state = {
  index: Venture.Index.t,
  ventureState,
  syncWorker: ref(SyncWorker.t),
  incomeWorker: ref(IncomeWorkerClient.t),
  persistWorker: ref(PersistWorkerClient.t),
};

let loadVentureAndIndex =
    (send, session: Session.t, currentRoute, {ventureState, persistWorker}) => {
  switch (session) {
  | LoggedIn({userId}) =>
    persistWorker^
    |. PersistWorkerClient.postMessage(
         PersistWorkerMessage.InitializeLocalStorage(
           userId,
           WorkerLocalStorage.readBlockstackItemsFromStorage(),
         ),
       );
    Js.Promise.(
      Venture.Index.load()
      |> then_(index => send(UpdateIndex(index)) |> resolve)
      |> ignore
    );
  | _ => ()
  };
  switch (session, currentRoute: Router.Config.route, ventureState) {
  | (LoggedIn(sessionData), Venture(id), VentureLoaded(venture))
      when id != (venture |> Venture.getId) =>
    Js.Global.setTimeout(
      () =>
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
        ),
      1,
    )
    |> ignore;
    LoadingVenture;
  | (LoggedIn(_), Venture(id), VentureLoaded(venture))
      when id == (venture |> Venture.getId) => ventureState
  | (LoggedIn(sessionData), Venture(id), _) =>
    Js.Global.setTimeout(
      () =>
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
        ),
      1,
    )
    |> ignore;
    LoadingVenture;
  | (LoggedIn(sessionData), JoinVenture(ventureId, userId), _) =>
    Js.Global.setTimeout(
      () =>
        Js.Promise.(
          Venture.join(
            sessionData,
            ~userId,
            ~ventureId,
            ~listenerState=ViewModel.make(),
            ~listener=ViewModel.apply,
          )
          |> then_(((index, venture)) => {
               send(UpdateVenture(VentureLoaded(venture)));
               ReasonReact.Router.push(
                 Router.Config.routeToUrl(
                   Router.Config.Venture(venture |> Venture.getId),
                 ),
               );
               send(UpdateIndex(index)) |> resolve;
             })
          |> ignore
        ),
      1,
    )
    |> ignore;
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
    incomeWorker: ref(IncomeWorkerClient.make(~onMessage=Js.log)),
    persistWorker: ref(PersistWorkerClient.make(~onMessage=Js.log)),
  },
  didMount: ({send, state}) =>
    loadVentureAndIndex(send, session, currentRoute, state) |> ignore,
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
        IncomeWorkerClient.terminate(state.incomeWorker^);
        let worker =
          IncomeWorkerClient.make(~onMessage=message =>
            send(IncomeWorkerMessage(message))
          );
        state.incomeWorker := worker;
        worker;
      },
      IncomeWorkerClient.terminate,
    ),
    Sub(
      () => {
        PersistWorkerClient.terminate(state.persistWorker^);
        let worker =
          PersistWorkerClient.make(~onMessage=message =>
            send(PersistWorkerMessage(message))
          );
        state.persistWorker := worker;
        worker;
      },
      PersistWorkerClient.terminate,
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
                    IncomeWorkerMessage.Wait
                    |> IncomeWorkerClient.postMessage(state.incomeWorker^)
                  }
                )
              )
          ),
        )
      }
    | PersistWorkerMessage(VenturePersisted(id)) =>
      Js.log("Venture '" ++ VentureId.toString(id) ++ "' persisted");
      ReasonReact.NoUpdate;
    | UpdateIndex(index) => ReasonReact.Update({...state, index})
    | UpdateVenture(ventureState) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, ventureState},
        (
          (_) =>
            Js.Global.setTimeout(
              () =>
                switch (ventureState) {
                | VentureLoaded(venture) =>
                  PersistWorkerMessage.PersistVenture(
                    venture |> Venture.getId,
                  )
                  |> PersistWorkerClient.postMessage(state.persistWorker^)
                  |> ignore;
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
                  IncomeWorkerMessage.MonitorAddresses(
                    venture |> Venture.Wallet.getExposedAddresses,
                    venture |> Venture.Wallet.getKnownTransactionIds,
                  )
                  |> IncomeWorkerClient.postMessage(state.incomeWorker^)
                  |> ignore;
                | _ =>
                  SyncWorker.Message.Wait
                  |> SyncWorker.postMessage(state.syncWorker^);
                  IncomeWorkerMessage.Wait
                  |> IncomeWorkerClient.postMessage(state.incomeWorker^);
                },
              1,
            )
            |> ignore
        ),
      )
    | CreateVenture(session, name) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, ventureState: CreatingVenture},
        (
          ({send}) =>
            Js.Global.setTimeout(
              () =>
                Js.Promise.(
                  Venture.Cmd.Create.exec(
                    session,
                    ~name,
                    ~listenerState=ViewModel.make(),
                    ~listener=ViewModel.apply,
                  )
                  |> then_(((newIndex, venture)) => {
                       send(UpdateVenture(VentureLoaded(venture)));
                       ReasonReact.Router.push(
                         Router.Config.routeToUrl(
                           Router.Config.Venture(venture |> Venture.getId),
                         ),
                       );
                       send(UpdateIndex(newIndex)) |> resolve;
                     })
                  |> ignore
                ),
              1,
            )
            |> ignore
        ),
      )
    },
  render: ({state: {index, ventureState}, send}) =>
    children(~index, ~selectedVenture=ventureState, ~updateVentureStore=send),
};
