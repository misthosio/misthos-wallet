open PrimitiveTypes;

open Event;

type t = {
  ventureId,
  network: Network.t,
  payoutPolicy: Policy.t,
  walletInfoCollector: WalletInfoCollector.t,
};

let make = () => {
  network: Network.Testnet,
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
  IncomeAddressExposed.make(
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
      {userId, masterKeyChain, network}: Session.Data.t,
      accountIdx,
      destinations,
      satsPerByte,
      {ventureId, payoutPolicy, walletInfoCollector},
    ) =>
  try (
    {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=
            walletInfoCollector
            |> WalletInfoCollector.nonReservedOldInputs(accountIdx, userId),
          ~allInputs=walletInfoCollector |> WalletInfoCollector.unusedInputs,
          ~destinations,
          ~satsPerByte,
          ~changeAddress=
            walletInfoCollector
            |> WalletInfoCollector.nextChangeAddress(accountIdx, userId),
          ~network,
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
            ~network,
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
