type status =
  | None
  | CreatingVenture(string);

type state = {newVenture: string};

type action =
  | ChangeNewVenture(string)
  | CreateVenture;

let component = ReasonReact.reducerComponent("VentureCreate");

let formText = event => ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event),
                        )##value;

let make =
    (
      ~selectedVenture: VentureStore.selectedVenture,
      ~onCreateVenture,
      _children,
    ) => {
  ...component,
  initialState: () => {newVenture: ""},
  reducer: (action, state) =>
    switch (action) {
    | ChangeNewVenture(text) => ReasonReact.Update({newVenture: text})
    | CreateVenture =>
      switch (String.trim(state.newVenture)) {
      | "" => ReasonReact.NoUpdate
      | name =>
        onCreateVenture(name);
        ReasonReact.Update({newVenture: ""});
      }
    },
  render: ({send, state}) =>
    MaterialUi.(
      switch (selectedVenture) {
      | CreatingVenture => <Spinner text="Creating venture" />
      | _ =>
        <Body2
          titles=["Create a Venture"]
          body1=
            <div>
              <Typography variant=`Body1>
                (
                  "Set up a new venture wallet with yourself as the initial partner. You can add and remove partners once the venture is established."
                  |> Utils.text
                )
              </Typography>
              <Typography variant=`Title>
                ("Venture Name" |> Utils.text)
              </Typography>
              <Input
                placeholder="Enter a Venture Name"
                value=(`String(state.newVenture))
                onChange=(e => send(ChangeNewVenture(formText(e))))
                autoFocus=true
                fullWidth=true
              />
              <MButton fullWidth=true onClick=(_e => send(CreateVenture))>
                ("create venture" |> Utils.text)
              </MButton>
            </div>
          body2=
            <div>
              <Typography variant=`Title>
                ("What can you do with a venture?" |> Utils.text)
              </Typography>
            </div>
        />
      }
    ),
};
