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

module Styles = {
  open Css;
  let fullHeight = style([height(`percent(100.0))]);
  let remainingHeight =
    style([height(`calc((`sub, `vh(60.0), `px(120))))]);
};

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
      <Grid container=true alignItems=`Flex_Start justify=`Center>
        <TitleBar>
          <Grid
            key="titleBar"
            container=true
            alignItems=`Flex_Start
            justify=`Center>
            <Grid item=true xs=V8>
              <Typography variant=`Headline>
                (Utils.text("Create a Venture"))
              </Typography>
            </Grid>
          </Grid>
        </TitleBar>
        (
          switch (selectedVenture) {
          | CreatingVenture => <Spinner text="Creating venture" />
          | _ =>
            <Grid
              container=true
              className=Styles.remainingHeight
              alignItems=`Flex_Start
              justify=`Center>
              <Grid className=Styles.fullHeight item=true xs=V8>
                <Grid
                  className=Styles.fullHeight
                  container=true
                  justify=`Space_Between>
                  <Grid className=Styles.fullHeight item=true xs=V5>
                    <Grid
                      className=Styles.fullHeight
                      container=true
                      direction=`Column
                      justify=`Space_Between
                      alignItems=`Stretch>
                      <Grid item=true>
                        <Typography variant=`Body1>
                          (
                            "Set up a new venture wallet with yourself as the initial partner. You can add and remove partners once the venture is established."
                            |> Utils.text
                          )
                        </Typography>
                      </Grid>
                      <Grid item=true>
                        <Typography variant=`Title>
                          ("Venture Name" |> Utils.text)
                        </Typography>
                      </Grid>
                      <Grid item=true>
                        <Input
                          placeholder="Enter a Venture Name"
                          value=(`String(state.newVenture))
                          onChange=(
                            e => send(ChangeNewVenture(formText(e)))
                          )
                          autoFocus=true
                          fullWidth=true
                        />
                      </Grid>
                      <Grid item=true>
                        <MButton
                          fullWidth=true onClick=(_e => send(CreateVenture))>
                          ("create venture" |> Utils.text)
                        </MButton>
                      </Grid>
                    </Grid>
                  </Grid>
                  <Grid item=true xs=V5>
                    <Typography variant=`Title>
                      ("What can you do with a venture?" |> Utils.text)
                    </Typography>
                  </Grid>
                </Grid>
              </Grid>
            </Grid>
          }
        )
      </Grid>
    ),
};
