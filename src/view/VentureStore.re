open PrimitiveTypes;

type action =
  | UpdateIndex(Venture.Index.t)
  | UpdateVenture(option(Venture.t(ViewModel.t)));

type state = {
  index: Venture.Index.t,
  loadedVenture: option(Venture.t(ViewModel.t)),
};

let loadVentureAndIndex =
    (
      send,
      session: Session.t,
      selectedVentureId: option(ventureId),
      {loadedVenture},
    ) =>
  switch (session, selectedVentureId) {
  | (LoggedIn(_), None) =>
    Js.Promise.(
      Venture.Index.load()
      |> then_(index => send(UpdateIndex(index)) |> resolve)
      |> ignore
    );
    loadedVenture;
  | (LoggedIn(sessionData), Some(id)) =>
    Js.Promise.(
      Venture.Index.load()
      |> then_(index => send(UpdateIndex(index)) |> resolve)
      |> ignore
    );
    if (Some(id)
        != (
             loadedVenture
             |> Utils.mapOption(v => Venture.getId(v) |> VentureId.fromString)
           )) {
      Js.Promise.(
        Venture.load(
          sessionData,
          ~ventureId=id,
          ~listenerState=ViewModel.make(),
          ~listener=ViewModel.apply,
        )
        |> then_(venture => send(UpdateVenture(Some(venture))) |> resolve)
        |> ignore
      );
      None;
    } else {
      loadedVenture;
    };
  | _ =>
    send(UpdateIndex([]));
    loadedVenture;
  };

let component = ReasonReact.reducerComponent("VentureStore");

let make =
    (~selectedVentureId: option(ventureId)=?, ~session: Session.t, children) => {
  ...component,
  initialState: () => {index: [], loadedVenture: None},
  didMount: ({send, state}) =>
    ReasonReact.Update({
      ...state,
      loadedVenture:
        loadVentureAndIndex(send, session, selectedVentureId, state),
    }),
  willReceiveProps: ({send, state}) => {
    ...state,
    loadedVenture:
      loadVentureAndIndex(send, session, selectedVentureId, state),
  },
  reducer: (action, state) =>
    switch (action) {
    | UpdateIndex(index) => ReasonReact.Update({...state, index})
    | UpdateVenture(venture) =>
      ReasonReact.Update({...state, loadedVenture: venture})
    },
  render: ({state: {index, loadedVenture}}) =>
    children(~index, ~selectedVenture=loadedVenture),
};
