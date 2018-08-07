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
  payoutPolicy: Policy.defaultPayout,
};

let apply = (event: Event.t, state) => {
  let state = {
    ...state,
    walletInfoCollector:
      state.walletInfoCollector |> WalletInfoCollector.apply(event),
  };
  switch (event) {
  | VentureCreated({ventureId, initialPolicies, network}) => {
      ...state,
      network,
      ventureId,
      payoutPolicy:
        initialPolicies
        |> Utils.mapOption((p: VentureCreated.initialPolicies) => p.payout)
        |> Js.Option.getWithDefault(Policy.defaultPayout),
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
      payoutTx,
      signatures,
      {ventureId, payoutPolicy, walletInfoCollector},
    ) =>
  try (
    {
      let payoutTx =
        switch (
          PayoutTransaction.signPayout(
            ~ventureId,
            ~userId,
            ~masterKeyChain,
            ~accountKeyChains=
              walletInfoCollector |> WalletInfoCollector.accountKeyChains,
            ~payoutTx,
            ~signatures,
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

let endorsePayout =
    (
      processId,
      signatures,
      {userId, masterKeyChain}: SessionData.t,
      {ventureId, walletInfoCollector},
    ) =>
  switch (
    PayoutTransaction.signPayout(
      ~ventureId,
      ~userId,
      ~masterKeyChain,
      ~accountKeyChains=
        walletInfoCollector |> WalletInfoCollector.accountKeyChains,
      ~payoutTx=
        walletInfoCollector |> WalletInfoCollector.getPayoutTx(processId),
      ~signatures,
    )
  ) {
  | Signed(payoutTx) => [
      PayoutSigned(
        Event.Payout.Signed.make(~processId, ~custodianId=userId, ~payoutTx),
      ),
      Event.makePayoutEndorsed(~processId, ~supporterId=userId),
    ]
  | NotSigned => [Event.makePayoutEndorsed(~processId, ~supporterId=userId)]
  };
