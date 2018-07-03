open PrimitiveTypes;

open Event;

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  walletInfoCollector: WalletInfoCollector.t,
};

let make = () => {
  network: Network.Regtest,
  ventureId: VentureId.fromString(""),
  walletInfoCollector: WalletInfoCollector.make(),
  payoutPolicy: Policy.unanimous,
};

let apply = (event: Event.t, state) => {
  let state = {
    ...state,
    walletInfoCollector:
      state.walletInfoCollector |> WalletInfoCollector.apply(event),
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

let exposeNextIncomeAddress = (userId, accountIdx, {walletInfoCollector}) => {
  let accountKeyChain =
    WalletInfoCollector.currentKeyChain(
      accountIdx,
      userId,
      walletInfoCollector,
    );
  let coordinates =
    Address.Coordinates.nextExternal(
      userId,
      walletInfoCollector |> WalletInfoCollector.exposedCoordinates,
      accountKeyChain,
    );
  Income.AddressExposed.make(
    ~partnerId=userId,
    ~address=Address.make(coordinates, accountKeyChain),
  );
};

type preparePayoutResult =
  | NotEnoughFunds
  | Ok(Payout.Proposed.t);

let preparePayoutTx =
    (
      ~eligibleWhenProposing,
      {userId, masterKeyChain}: SessionData.t,
      accountIdx,
      destinations,
      satsPerByte,
      {ventureId, payoutPolicy, walletInfoCollector},
    ) =>
  try (
    {
      let payoutTx =
        PayoutTransaction.build(
          ~optionalInputs=
            walletInfoCollector
            |> WalletInfoCollector.currentSpendableInputs(accountIdx),
          ~mandatoryInputs=
            walletInfoCollector
            |> WalletInfoCollector.oldSpendableInputs(accountIdx),
          ~destinations,
          ~satsPerByte,
          ~changeAddress=
            walletInfoCollector
            |> WalletInfoCollector.nextChangeAddress(accountIdx, userId),
          ~network=walletInfoCollector |> WalletInfoCollector.network,
        );
      let payoutTx =
        switch (
          PayoutTransaction.signPayout(
            ~ventureId,
            ~userId,
            ~masterKeyChain,
            ~accountKeyChains=
              walletInfoCollector |> WalletInfoCollector.accountKeyChains,
            ~payoutTx,
          )
        ) {
        | Signed(payout) => payout
        | NotSigned => payoutTx
        };
      Ok(
        Event.Payout.(
          Proposed.make(
            ~eligibleWhenProposing,
            ~proposerId=userId,
            ~policy=payoutPolicy,
            Data.{accountIdx, payoutTx},
          )
        ),
      );
    }
  ) {
  | PayoutTransaction.NotEnoughFunds => NotEnoughFunds
  };
