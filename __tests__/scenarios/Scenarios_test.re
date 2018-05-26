open Jest;

open Expect;

let () =
  Scenarios.run("income-summary", viewModel => {
    let createPayoutModal = viewModel |> ViewModel.createPayoutModal;
    let balance = createPayoutModal.balance;
    test("Balance is correct", () =>
      expect(balance.currentSpendable)
      |> toEqual(BTC.fromSatoshis(50756770L))
    );
    /* test("Can create summary", () => { */
    /*   let summary = createPayoutModal.summary([], BTC.fromSatoshis(100L)); */
    /*   Js.log(summary.misthosFee); */
    /*   expect(true) |> toEqual(true); */
    /* }) */
  });
