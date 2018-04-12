open PrimitiveTypes;

type action =
  | UpdateSession(Session.t)
  | SignIn
  | SignOut;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("UserSession");

let make = children => {
  ...component,
  initialState: () => {session: Session.Unknown},
  didMount: (_) =>
    ReasonReact.SideEffects(
      ({send}) =>
        Js.Promise.(
          Session.getCurrentSession()
          |> then_(session => send(UpdateSession(session)) |> resolve)
          |> ignore
        ),
    ),
  reducer: (action, state) =>
    switch (action) {
    | UpdateSession(session) => ReasonReact.Update({...state, session})
    | SignIn => ReasonReact.Update({...state, session: Session.signIn()})
    | SignOut => ReasonReact.Update({...state, session: Session.signOut()})
    },
  render: ({state, send, handle}) =>
    children(~session=state.session, ~updateSession=send),
};
