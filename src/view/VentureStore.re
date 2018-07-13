open PrimitiveTypes;

type selectedVenture =
  | None
  | CreatingVenture(CommandExecutor.cmdStatus)
  | JoiningVenture(ventureId, CommandExecutor.cmdStatus)
  | LoadingVenture(ventureId, CommandExecutor.cmdStatus)
  | VentureLoaded(ventureId, ViewModel.t, VentureWorkerClient.Cmd.t);

type action =
  | CreateVenture(string)
  | TabSync(VentureWorkerMessage.outgoing)
  | DataWorkerMessage(DataWorkerMessage.outgoing)
  | VentureWorkerMessage(VentureWorkerMessage.outgoing);

type state = {
  index: option(Venture.Index.t),
  selectedVenture,
  session: Session.t,
  dataWorker: ref(DataWorkerClient.t),
  persistWorker: ref(PersistWorkerClient.t),
  ventureWorker: ref(VentureWorkerClient.t),
};

let loadVentureAndIndex =
    (
      session: Session.t,
      currentRoute,
      {selectedVenture, ventureWorker, session: oldSession},
    ) => {
  if (oldSession != session) {
    ventureWorker^ |> VentureWorkerClient.updateSession;
  };
  switch (session, currentRoute: Router.Config.route, selectedVenture) {
  | (LoggedIn(_), Venture(ventureId, _), VentureLoaded(loadedId, _, _))
      when VentureId.eq(ventureId, loadedId) => selectedVenture
  | (LoggedIn(_), Venture(ventureId, _), VentureLoaded(loadedId, _, _))
      when VentureId.neq(ventureId, loadedId) =>
    LoadingVenture(
      ventureId,
      Pending(ventureWorker^ |> VentureWorkerClient.load(~ventureId)),
    )
  | (LoggedIn(_), Venture(ventureId, _), _) =>
    LoadingVenture(
      ventureId,
      Pending(ventureWorker^ |> VentureWorkerClient.load(~ventureId)),
    )
  | (LoggedIn(_sessionData), JoinVenture(ventureId, userId), _) =>
    JoiningVenture(
      ventureId,
      Pending(
        ventureWorker^ |> VentureWorkerClient.joinVia(~ventureId, ~userId),
      ),
    )
  | _ => None
  };
};

let component = ReasonReact.reducerComponent("VentureStore");

module L = Dom.Storage;

let updateOtherTabs = msg => {
  let encodedMsg = msg |> VentureWorkerMessage.encodeOutgoing;
  L.localStorage |> L.setItem("tab-sync", encodedMsg |> Json.stringify);
};

external toStorageEvent : Dom.event => StorageEventRe.t = "%identity";

let handler = (send, msg) => {
  let storageEvent = msg |> toStorageEvent;
  switch (storageEvent |> StorageEventRe.key) {
  | "tab-sync" =>
    try (
      TabSync(
        storageEvent
        |> StorageEventRe.newValue
        |> Json.parseOrRaise
        |> VentureWorkerMessage.decodeOutgoing,
      )
      |> send
    ) {
    | _ => ()
    }
  | _ => ()
  };
};

let make = (~currentRoute, ~session: Session.t, children) => {
  ...component,
  initialState: () => {
    session: Unknown,
    index: None,
    selectedVenture: None,
    dataWorker: ref(DataWorkerClient.make(~onMessage=Js.log)),
    persistWorker: ref(PersistWorkerClient.make(~onMessage=Js.log)),
    ventureWorker: ref(VentureWorkerClient.make(~onMessage=Js.log)),
  },
  didMount: ({state}) =>
    loadVentureAndIndex(session, currentRoute, state) |> ignore,
  willReceiveProps: ({state}) => {
    ...state,
    selectedVenture: loadVentureAndIndex(session, currentRoute, state),
    session,
  },
  subscriptions: ({send, state}) => {
    let eventListener = handler(send);
    [
      Sub(
        () => {
          DomRe.window |> WindowRe.addEventListener("storage", eventListener);
          eventListener;
        },
        listener =>
          DomRe.window |> WindowRe.removeEventListener("storage", listener),
      ),
      Sub(
        () => {
          DataWorkerClient.terminate(state.dataWorker^);
          let worker =
            DataWorkerClient.make(~onMessage=message =>
              send(DataWorkerMessage(message))
            );
          state.dataWorker := worker;
          worker;
        },
        DataWorkerClient.terminate,
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
    ];
  },
  reducer: (action, state) =>
    switch (state.session) {
    | LoggedIn(sessionData) =>
      switch (action) {
      | CreateVenture(name) =>
        let createCmdId =
          state.ventureWorker^ |> VentureWorkerClient.create(~name);
        ReasonReact.Update({
          ...state,
          selectedVenture: CreatingVenture(Pending(createCmdId)),
        });
      | VentureWorkerMessage(msg) =>
        state.persistWorker^ |. PersistWorkerClient.postMessage(msg) |> ignore;
        state.dataWorker^ |. DataWorkerClient.postMessage(msg) |> ignore;
        switch (msg, state.selectedVenture) {
        | (
            CmdCompleted(_, correlationId, response),
            CreatingVenture(Pending(createCmdId)),
          )
            when correlationId == createCmdId =>
          ReasonReact.Update({
            ...state,
            selectedVenture:
              CreatingVenture(
                switch (response) {
                | Ok(success) => Success(success)
                | Error(error) => Error(error)
                },
              ),
          })
        | (
            CmdCompleted(_, correlationId, response),
            JoiningVenture(ventureId, Pending(joinCmdId)),
          )
            when correlationId == joinCmdId =>
          ReasonReact.Update({
            ...state,
            selectedVenture:
              JoiningVenture(
                ventureId,
                switch (response) {
                | Ok(success) => Success(success)
                | Error(error) => Error(error)
                },
              ),
          })
        | (
            CmdCompleted(_, correlationId, response),
            LoadingVenture(ventureId, Pending(joinCmdId)),
          )
            when correlationId == joinCmdId =>
          ReasonReact.Update({
            ...state,
            selectedVenture:
              LoadingVenture(
                ventureId,
                switch (response) {
                | Ok(success) => Success(success)
                | Error(error) => Error(error)
                },
              ),
          })
        | (VentureCreated(ventureId, log), _) =>
          ReasonReact.UpdateWithSideEffects(
            {
              ...state,
              selectedVenture:
                VentureLoaded(
                  ventureId,
                  ViewModel.init(sessionData.userId, log),
                  VentureWorkerClient.Cmd.make(
                    state.ventureWorker^,
                    ventureId,
                  ),
                ),
            },
            (_ => Router.goTo(Router.Config.Venture(ventureId, None))),
          )
        | (UpdateIndex(index), _) =>
          updateOtherTabs(msg);
          ReasonReact.Update({...state, index: Some(index)});
        | (VentureLoaded(ventureId, log, _), JoiningVenture(joiningId, _))
            when VentureId.eq(ventureId, joiningId) =>
          ReasonReact.UpdateWithSideEffects(
            {
              ...state,
              selectedVenture:
                VentureLoaded(
                  ventureId,
                  ViewModel.init(sessionData.userId, log),
                  VentureWorkerClient.Cmd.make(
                    state.ventureWorker^,
                    ventureId,
                  ),
                ),
            },
            (_ => Router.goTo(Router.Config.Venture(ventureId, None))),
          )
        | (
            VentureLoaded(ventureId, events, _),
            LoadingVenture(loadingId, _),
          )
            when VentureId.eq(ventureId, loadingId) =>
          ReasonReact.Update({
            ...state,
            selectedVenture:
              VentureLoaded(
                ventureId,
                ViewModel.init(sessionData.userId, events),
                VentureWorkerClient.Cmd.make(state.ventureWorker^, ventureId),
              ),
          })
        | (
            NewItems(ventureId, newItems),
            VentureLoaded(loadedId, viewModel, cmd),
          )
            when VentureId.eq(ventureId, loadedId) =>
          updateOtherTabs(msg);
          ReasonReact.Update({
            ...state,
            selectedVenture:
              VentureLoaded(
                ventureId,
                viewModel |> ViewModel.applyAll(newItems),
                cmd,
              ),
          });
        | (
            CmdCompleted(ventureId, correlationId, response),
            VentureLoaded(loadedId, viewModel, cmd),
          )
            when VentureId.eq(ventureId, loadedId) =>
          ReasonReact.Update({
            ...state,
            selectedVenture:
              VentureLoaded(
                loadedId,
                viewModel
                |> ViewModel.captureResponse(correlationId, response),
                cmd,
              ),
          })
        | _ => ReasonReact.NoUpdate
        };
      | DataWorkerMessage(msg) =>
        state.ventureWorker^ |. VentureWorkerClient.postMessage(msg) |> ignore;
        ReasonReact.NoUpdate;
      | TabSync(msg) =>
        switch (msg) {
        | NewItems(ventureId, newItems) =>
          state.ventureWorker^
          |. VentureWorkerClient.postMessage(SyncTabs(ventureId, newItems))
          |> ignore;
          switch (state.selectedVenture) {
          | VentureLoaded(loadedId, viewModel, cmd)
              when VentureId.eq(loadedId, ventureId) =>
            ReasonReact.Update({
              ...state,
              selectedVenture:
                VentureLoaded(
                  ventureId,
                  viewModel |> ViewModel.applyAll(newItems),
                  cmd,
                ),
            })
          | _ => ReasonReact.NoUpdate
          };
        | UpdateIndex(index) =>
          ReasonReact.Update({...state, index: Some(index)})
        | _ => ReasonReact.NoUpdate
        }
      }
    | _ =>
      switch (action) {
      | VentureWorkerMessage(msg) =>
        state.persistWorker^ |. PersistWorkerClient.postMessage(msg) |> ignore;
        state.dataWorker^ |. DataWorkerClient.postMessage(msg) |> ignore;
      | _ => ()
      };
      ReasonReact.NoUpdate;
    },
  render: ({state: {index, selectedVenture}, send}) =>
    children(~index, ~selectedVenture, ~createVenture=name =>
      send(CreateVenture(name))
    ),
};
