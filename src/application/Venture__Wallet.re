open PrimitiveTypes;

open WalletTypes;

open Event;

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  txInputCollector: TxInputCollector.t,
  activatedKeyChain:
    list((accountIdx, list((userId, AccountKeyChain.Identifier.t)))),
  exposedCoordinates: list(Address.Coordinates.t),
};

let make = () => {
  network: Network.Testnet,
  ventureId: VentureId.fromString(""),
  txInputCollector: TxInputCollector.make(),
  payoutPolicy: Policy.unanimous,
  activatedKeyChain: [],
  exposedCoordinates: [],
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
  | AccountCreationAccepted(
      ({data: {accountIdx}}: AccountCreation.Accepted.t),
    ) => {
      ...state,
      activatedKeyChain: [(accountIdx, []), ...state.activatedKeyChain],
    }
  | AccountKeyChainActivated({accountIdx, custodianId, identifier}) => {
      ...state,
      activatedKeyChain: [
        (
          accountIdx,
          [
            (custodianId, identifier),
            ...state.activatedKeyChain |> List.assoc(accountIdx),
          ],
        ),
        ...state.activatedKeyChain |> List.remove_assoc(accountIdx),
      ],
    }
  | IncomeAddressExposed(({coordinates}: IncomeAddressExposed.t)) => {
      ...state,
      exposedCoordinates: [coordinates, ...state.exposedCoordinates],
    }
  | PayoutProposed({data}) => {
      ...state,
      exposedCoordinates:
        switch (data.changeAddressCoordinates) {
        | None => state.exposedCoordinates
        | Some(coordinates) => [coordinates, ...state.exposedCoordinates]
        },
    }
  | _ => state
  };
};

let exposeNextIncomeAddress =
    (
      userId,
      accountIdx,
      {
        exposedCoordinates,
        activatedKeyChain,
        txInputCollector: {keyChains: accountKeyChains},
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
        exposedCoordinates,
        activatedKeyChain,
        txInputCollector: {keyChains: accountKeyChains, unused: inputs},
      },
    ) => {
  open Address;
  let keyChainIdent =
    activatedKeyChain |> List.assoc(accountIdx) |> List.assoc(userId);
  let accountKeyChain =
    accountKeyChains
    |> AccountKeyChain.Collection.lookup(accountIdx, keyChainIdent);
  let coordinates =
    exposedCoordinates |> Coordinates.allForAccount(accountIdx);
  let nextChangeCoordinates =
    Coordinates.nextInternal(userId, coordinates, accountKeyChain);
  let oldInputs =
    inputs
    |. Belt.Set.keepU((. i: Network.txInput) =>
         i.coordinates
         |> Coordinates.keyChainIdent
         |> AccountKeyChain.Identifier.neq(keyChainIdent)
       );
  let changeAddress = Address.find(nextChangeCoordinates, accountKeyChains);
  try (
    {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=oldInputs,
          ~allInputs=inputs,
          ~destinations,
          ~satsPerByte,
          ~changeAddress,
          ~network,
        );
      let changeAddressCoordinates =
        payoutTx.changeAddress
        |> Utils.mapOption((_) => nextChangeCoordinates);
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
};
