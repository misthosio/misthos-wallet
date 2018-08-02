include ViewCommon;

type action =
  | TransportCreated(Ledger.transport)
  | TransportCreationFailed(Ledger.transportError)
  | HDNodeFetched(Bitcoin.HDNode.t)
  | FailedToFetchHDNode(Ledger.transportError);

type connectionStatus =
  | Connecting
  | ConnectionFailed(Ledger.transportError)
  | Connected(Bitcoin.HDNode.t);
type state = {connectionStatus};

let createConnection = send => {
  Js.log("createConnection");
  Js.Promise.(
    Ledger.createBrowserTransport()
    |> then_(transport => transport |. TransportCreated |> send |> resolve)
    |> catch(e => {
         Js.log(e);
         send(TransportCreationFailed(Ledger.decodeTransportError(e)))
         |> resolve;
       })
    |> ignore
  );
};

let component = ReasonReact.reducerComponent("ConnectLedger");
let make = _children => {
  let statusToString =
    fun
    | Connecting => "Connecting"
    | ConnectionFailed(error) =>
      "ConnectionFailed(" ++ Ledger.transportErrorToString(error) ++ ")"
    | Connected(_) => "Connected";
  {
    ...component,
    didMount: ({send}) => createConnection(send),
    initialState: () => {connectionStatus: Connecting},
    reducer: (action, _state) =>
      switch (action) {
      | TransportCreated(transport) =>
        let btc = Ledger.btc(transport);
        Js.log("Connected");
        ReasonReact.SideEffects(
          (
            ({send}) =>
              Js.Global.setTimeout(
                () =>
                  Js.Promise.(
                    btc
                    |> Ledger.getHDNode(
                         "44'/0'/0'/0",
                         Bitcoin.Networks.bitcoin,
                       )
                    |> then_(hdNode =>
                         send(HDNodeFetched(hdNode)) |> resolve
                       )
                    |> catch(error => {
                         Js.log(error);
                         send(
                           FailedToFetchHDNode(
                             error |> Ledger.decodeTransportError,
                           ),
                         )
                         |> resolve;
                       })
                  )
                  |> ignore,
                0,
              )
              |> ignore
          ),
        );
      | TransportCreationFailed(error) =>
        ReasonReact.Update({connectionStatus: ConnectionFailed(error)})
      | FailedToFetchHDNode(error) =>
        ReasonReact.Update({connectionStatus: ConnectionFailed(error)})
      | HDNodeFetched(hdNode) =>
        Js.log(
          hdNode
          |> Bitcoin.HDNode.getPublicKey
          |. Bitcoin.ECPair.fromPublicKey({
               "network": hdNode |> Bitcoin.HDNode.getNetwork,
             })
          |> Bitcoin.Address.fromKeyPair,
        );
        ReasonReact.Update({connectionStatus: Connected(hdNode)});
      },
    render: ({state}) =>
      <Grid
        title1=("Connect Ledger" |> text)
        area3={
          <div>
            ("status" ++ statusToString(state.connectionStatus) |> text)
          </div>
        }
      />,
  };
};
