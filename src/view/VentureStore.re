type ventureState =
  | None
  | CreatingVenture
  | JoiningVenture
  | LoadingVenture
  | VentureLoaded(Venture.t(ViewModel.t));

type action =
  | UpdateIndex(Venture.Index.t)
  | UpdateVenture(ventureState)
  | CreateVenture(Session.Data.t, string);

type state = {
  index: Venture.Index.t,
  ventureState,
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
  initialState: () => {index: [], ventureState: None},
  didMount: ({send, state}) =>
    ReasonReact.Update({
      ...state,
      ventureState: loadVentureAndIndex(send, session, currentRoute, state),
    }),
  willReceiveProps: ({send, state}) => {
    ...state,
    ventureState: loadVentureAndIndex(send, session, currentRoute, state),
  },
  reducer: (action, state) =>
    switch (action) {
    | UpdateIndex(index) => ReasonReact.Update({...state, index})
    | UpdateVenture(venture) =>
      ReasonReact.Update({...state, ventureState: venture})
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
