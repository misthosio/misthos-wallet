include ViewCommon;

module ViewData = ViewModel.LedgerKeysView;

type action =
  | SubmitPubKeys
  | FailedGettingKeys(LedgerJS.error)
  | Completed(Bitcoin.HDNode.t);

type status =
  | Idle
  | InProgress
  | Completed;
type state = {status};

let component = ReasonReact.reducerComponent("ConnectLedger");
let make = (~viewData: ViewData.t, _children) => {
  ...component,
  initialState: () => {status: Idle},
  reducer: (action, _state: state) =>
    switch (action) {
    | SubmitPubKeys =>
      ReasonReact.UpdateWithSideEffects(
        {status: InProgress},
        (({send}) => viewData.getCustodianKeyChain() |> ignore),
      )
    /* | TransportCreated(transport) => */
    /*   let btc = Ledger.btc(transport); */
    /*   Js.log("Connected"); */
    /*   ReasonReact.SideEffects( */
    /*     ( */
    /*       ({send}) => */
    /*         Js.Global.setTimeout( */
    /*           () => */
    /*             Js.Promise.( */
    /*               btc */
    /*               |> Ledger.getHDNode( */
    /*                    "44'/0'/0'/0", */
    /*                    Bitcoin.Networks.bitcoin, */
    /*                  ) */
    /*               |> then_(hdNode => send(HDNodeFetched(hdNode)) |> resolve) */
    /*               |> catch(error => { */
    /*                    Js.log(error); */
    /*                    send( */
    /*                      FailedToFetchHDNode( */
    /*                        error |> Ledger.decodeTransportError, */
    /*                      ), */
    /*                    ) */
    /*                    |> resolve; */
    /*                  }) */
    /*             ) */
    /*             |> ignore, */
    /*           0, */
    /*         ) */
    /*         |> ignore */
    /*     ), */
    /*   ); */
    /* | TransportCreationFailed(error) => */
    /*   ReasonReact.Update({connectionStatus: ConnectionFailed(error)}) */
    /* | FailedToFetchHDNode(error) => */
    /*   ReasonReact.Update({connectionStatus: ConnectionFailed(error)}) */
    /* | HDNodeFetched(hdNode) => */
    /*   Js.log( */
    /*     hdNode */
    /*     |> Bitcoin.HDNode.getPublicKey */
    /*     |. Bitcoin.ECPair.fromPublicKey({ */
    /*          "network": hdNode |> Bitcoin.HDNode.getNetwork, */
    /*        }) */
    /*     |> Bitcoin.Address.fromKeyPair, */
    /*   ); */
    /*   ReasonReact.Update({connectionStatus: Connected(hdNode)}); */
    },
  render: ({state, send}) =>
    <Grid
      title1=("Connect Ledger" |> text)
      area3={
        <div>
          <MButton onClick=(ignoreEvent(() => send(SubmitPubKeys))) />
        </div>
      }
    />,
};
