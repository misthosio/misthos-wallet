open PrimitiveTypes;

open Event;

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  txInputCollector: TxInputCollector.t,
};

let make = () => {
  network: Network.Testnet,
  ventureId: VentureId.fromString(""),
  txInputCollector: TxInputCollector.make(),
  payoutPolicy: Policy.unanimous,
};

let apply = (event: Event.t, state) => {
  let state = {
    ...state,
    txInputCollector: state.txInputCollector |> TxInputCollector.apply(event),
  };
  switch (event) {
  | VentureCreated({ventureId, metaPolicy, network}) => {
      ...state,
      network,
      ventureId,
      payoutPolicy: metaPolicy,
    }
  | _ => state
  };
};

let exposeNextIncomeAddress =
    (
      userId,
      accountIdx,
      {
        txInputCollector: {
          exposedCoordinates,
          activatedKeyChain,
          keyChains: accountKeyChains,
        },
      },
    ) => {
  let ident =
    activatedKeyChain |> List.assoc(accountIdx) |> List.assoc(userId);
  let accountKeyChain =
    accountKeyChains |> AccountKeyChain.Collection.lookup(accountIdx, ident);
  let coordinates =
    Address.Coordinates.nextExternal(
      userId,
      exposedCoordinates,
      accountKeyChain,
    );
  IncomeAddressExposed.make(
    ~coordinates,
    ~address=Address.make(coordinates, accountKeyChain).address,
  );
};

type preparePayoutResult =
  | NotEnoughFunds
  | Ok(Payout.Proposed.t);

let preparePayoutTx =
    (
      {userId, masterKeyChain, network}: Session.Data.t,
      accountIdx,
      destinations,
      satsPerByte,
      {
        ventureId,
        payoutPolicy,
        txInputCollector:
          {keyChains: accountKeyChains, unused: inputs} as txInputCollector,
      },
    ) =>
  try (
    {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=
            txInputCollector |> TxInputCollector.oldInputs(accountIdx, userId),
          ~allInputs=inputs,
          ~destinations,
          ~satsPerByte,
          ~changeAddress=
            txInputCollector
            |> TxInputCollector.nextChangeAddress(accountIdx, userId),
          ~network,
        );
      let changeAddressCoordinates =
        payoutTx.changeAddress
        |> Utils.mapOption(((_, coordinates)) => coordinates);
      let payoutTx =
        switch (
          PayoutTransaction.signPayout(
            ~ventureId,
            ~userId,
            ~masterKeyChain,
            ~accountKeyChains,
            ~payoutTx,
            ~network,
          )
        ) {
        | Signed(payout) => payout
        | NotSigned => payoutTx
        };
      Ok(
        Event.Payout.(
          Proposed.make(
            ~supporterId=userId,
            ~policy=payoutPolicy,
            Data.{accountIdx, payoutTx, changeAddressCoordinates},
          )
        ),
      );
    }
  ) {
  | PayoutTransaction.NotEnoughFunds => NotEnoughFunds
  };
