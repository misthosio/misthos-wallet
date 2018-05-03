open PrimitiveTypes;

type selectedVenture =
  | None
  | CreatingVenture
  | JoiningVenture
  | LoadingVenture(ventureId)
  | VentureLoaded(ventureId, ViewModel.t, VentureWorkerClient.Cmd.t);

type action =
  | CreateVenture(string)
  | SyncWorkerMessage(SyncWorker.Message.receive)
  | IncomeWorkerMessage(IncomeWorkerMessage.receive)
  | VentureWorkerMessage(VentureWorkerMessage.receive);

type state = {
  index: option(Venture.Index.t),
  selectedVenture,
  syncWorker: ref(SyncWorker.t),
  incomeWorker: ref(IncomeWorkerClient.t),
  persistWorker: ref(PersistWorkerClient.t),
  ventureWorker: ref(VentureWorkerClient.t),
};

let loadVentureAndIndex =
    (
      session: Session.t,
      currentRoute,
      {selectedVenture, ventureWorker, persistWorker},
    ) => {
  ventureWorker^ |> VentureWorkerClient.updateSession;
  switch (session) {
  | LoggedIn({userId}) =>
    persistWorker^ |> PersistWorkerClient.updateSession(userId)
  | _ => ()
  };
  switch (session, currentRoute: Router.Config.route, selectedVenture) {
  | (LoggedIn(_), Venture(ventureId), VentureLoaded(loadedId, _, _))
      when VentureId.eq(ventureId, loadedId) => selectedVenture
  | (LoggedIn(_), Venture(ventureId), VentureLoaded(loadedId, _, _))
      when VentureId.neq(ventureId, loadedId) =>
    ventureWorker^ |> VentureWorkerClient.load(~ventureId);
    LoadingVenture(ventureId);
  | (LoggedIn(_), Venture(ventureId), _) =>
    ventureWorker^ |> VentureWorkerClient.load(~ventureId);
    LoadingVenture(ventureId);
  | (LoggedIn(_sessionData), JoinVenture(_ventureId, _userId), _) =>
    JoiningVenture
  | _ => None
  };
};

let component = ReasonReact.reducerComponent("VentureStore");

let make = (~currentRoute, ~session: Session.t, children) => {
  ...component,
  initialState: () => {
    index: None,
    selectedVenture: None,
    syncWorker: ref(SyncWorker.make(~onMessage=Js.log)),
    incomeWorker: ref(IncomeWorkerClient.make(~onMessage=Js.log)),
    persistWorker: ref(PersistWorkerClient.make(~onMessage=Js.log)),
    ventureWorker: ref(VentureWorkerClient.make(~onMessage=Js.log)),
  },
  didMount: ({state}) =>
    loadVentureAndIndex(session, currentRoute, state) |> ignore,
  willReceiveProps: ({state}) => {
    ...state,
    selectedVenture: loadVentureAndIndex(session, currentRoute, state),
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
    Sub(() => state.persistWorker^, PersistWorkerClient.terminate),
    Sub(
      () => {
        VentureWorkerClient.terminate(state.ventureWorker^);
        let worker =
          VentureWorkerClient.make(~onMessage=message =>
            send(VentureWorkerMessage(message))
          );
        state.ventureWorker := worker;
        worker;
      },
      VentureWorkerClient.terminate,
    ),
  ],
  reducer: (action, state) =>
    switch (action) {
    | CreateVenture(name) =>
      state.ventureWorker^ |> VentureWorkerClient.create(~name);
      ReasonReact.Update({...state, selectedVenture: CreatingVenture});
    | VentureWorkerMessage(msg) =>
      state.persistWorker^ |> PersistWorkerClient.ventureMessage(msg);
      switch (msg, state.selectedVenture) {
      | (UpdateIndex(index), _) =>
        ReasonReact.Update({...state, index: Some(index)})
      | (VentureCreated(ventureId, events), _) =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            selectedVenture:
              VentureLoaded(
                ventureId,
                ViewModel.init(events),
                VentureWorkerClient.Cmd.make(state.ventureWorker^, ventureId),
              ),
          },
          ((_) => Router.goTo(Router.Config.Venture(ventureId))),
        )
      | (VentureLoaded(ventureId, events), LoadingVenture(loadingId))
          when VentureId.eq(ventureId, loadingId) =>
        ReasonReact.Update({
          ...state,
          selectedVenture:
            VentureLoaded(
              ventureId,
              ViewModel.init(events),
              VentureWorkerClient.Cmd.make(state.ventureWorker^, ventureId),
            ),
        })
      | (
          NewEvents(ventureId, newEvents),
          VentureLoaded(loadedId, viewModel, cmd),
        )
          when VentureId.eq(ventureId, loadedId) =>
        ReasonReact.Update({
          ...state,
          selectedVenture:
            VentureLoaded(
              ventureId,
              viewModel |> ViewModel.applyAll(newEvents),
              cmd,
            ),
        })
      | _ => ReasonReact.NoUpdate
      };
    | SyncWorkerMessage(Fetched(_eventLogs)) => ReasonReact.NoUpdate
    | IncomeWorkerMessage(_msg) => ReasonReact.NoUpdate
    },
  render: ({state: {index, selectedVenture}, send}) =>
    children(~index, ~selectedVenture, ~createVenture=name =>
      send(CreateVenture(name))
    ),
};
