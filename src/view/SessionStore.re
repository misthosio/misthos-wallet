type action =
  | UpdateSession(Session.t)
  | SignIn
  | SignOut;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("SessionStore");

let make = children => {
  ...component,
  initialState: () => {session: Session.Unknown},
  didMount: ({send}) =>
    Js.Promise.(
      Session.getCurrentSession(~environment=Env.getEnvironment(), ())
      |> then_(session => send(UpdateSession(session)) |> resolve)
      |> ignore
    ),
  reducer: (action, _state) =>
    switch (action) {
    | UpdateSession(session) => ReasonReact.Update({session: session})
    | SignIn =>
      ReasonReact.Update({
        session: Session.signIn(~environment=Env.getEnvironment(), ()),
      })
    | SignOut => ReasonReact.Update({session: Session.signOut()})
    },
  render: ({state, send}) =>
    children(~session=state.session, ~updateSession=send),
};
