type action =
  | UpdateSession(Session.t)
  | SignIn
  | SignOut
  | SignTAC;

type state = {session: Session.t};

let component = ReasonReact.reducerComponent("SessionStore");

let make = children => {
  ...component,
  initialState: () => {session: Session.Unknown},
  didMount: ({send}) =>
    Js.Promise.(
      Session.getCurrentSession()
      |> then_(session => send(UpdateSession(session)) |> resolve)
      |> ignore
    ),
  reducer: (action, state) =>
    switch (action) {
    | UpdateSession(session) => ReasonReact.Update({session: session})
    | SignIn => ReasonReact.Update({session: Session.signIn()})
    | SignOut => ReasonReact.Update({session: Session.signOut()})
    | SignTAC =>
      switch (state.session) {
      | MustAggreeToTAC(session, userInfo) =>
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Promise.(
                UserInfo.signTAC(
                  TACText.hash,
                  session.appPrivateKey,
                  session.network,
                  userInfo,
                )
                |> then_(_ =>
                     send(UpdateSession(LoggedIn(session))) |> resolve
                   )
                |> ignore
              )
          ),
        )
      | _ => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) =>
    children(~session=state.session, ~updateSession=send, ~signTAC=_ =>
      send(SignTAC)
    ),
};
