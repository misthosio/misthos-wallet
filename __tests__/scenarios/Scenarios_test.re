open Jest;

open Expect;

let () =
  Scenarios.run("income-summary", viewModel => {
    let createPayoutModal = viewModel |> ViewModel.createPayoutModal;
    test("Balance is correct", () => {
      let balance = createPayoutModal.balance;
      expect(balance.currentSpendable)
      |> toEqual(BTC.fromSatoshis(50756770L));
    });
    test("Network fee is set", () => {
      let viewData = createPayoutModal;
      let createPayoutModalState: CreatePayoutModal.state = {
        frozen: false,
        viewData,
        canSubmitProposal: false,
        destinations: [],
        addressValid: true,
        summary: viewData.initialSummary,
        inputDestination: "",
        inputAmount: BTC.zero,
        inputs: {
          recipientAddress: "",
          btcAmount: "2MvTochgBg25bYVyJwaX6nWM1AThbmaoUHA",
        },
      };
      let updatedState =
        CreatePayoutModal.updateState(createPayoutModalState);
      expect(updatedState.summary.networkFee)
      |> toEqual(BTC.fromSatoshis(184650L));
    });
  });
