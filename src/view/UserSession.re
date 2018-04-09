open PrimitiveTypes;

type action =
  | LoginCompleted(Session.t)
  | SignIn
  | SignOut;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("UserSession");

let make = children => {
  ...component,
  initialState: () => {session: Session.getCurrentSession()},
  didMount: ({state}) =>
    switch (state.session) {
    | LoginPending =>
      ReasonReact.SideEffects(
        (
          ({send}) =>
            Js.Promise.(
              Session.completeLogIn()
              |> then_(session => send(LoginCompleted(session)) |> resolve)
              |> ignore
            )
        ),
      )
    | _ => ReasonReact.NoUpdate
    },
  reducer: (action, state) =>
    switch (action) {
    | LoginCompleted(session) => ReasonReact.Update({...state, session})
    | SignIn => ReasonReact.Update({...state, session: Session.signIn()})
    | SignOut => ReasonReact.Update({...state, session: Session.signOut()})
    },
  render: ({state, send, handle}) =>
    children(~session=state.session, ~updateSession=send),
};
