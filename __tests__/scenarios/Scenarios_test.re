open Belt;

open Jest;
open Expect;

let () = {
  Scenarios.run("three-person-payout", (_venture, newItems) => {
    test("There are 2 new Items", () =>
      expect(newItems |. Array.length) |> toEqual(2)
    );
    test("Payout is finalized", () => {
      let lastEvent = (newItems |. Array.getExn(1)).event;
      expect(
        switch (lastEvent) {
        | Event.PayoutFinalized(_) => true
        | _ => false
        },
      )
      |> toEqual(true);
    });
  });
  Scenarios.run("four-person-payout", (_venture, newItems) => {
    test("There are 3 new Items", () =>
      expect(newItems |. Array.length) |> toEqual(3)
    );
    test("Payout is finalized", () => {
      let lastEvent = (newItems |. Array.getExn(1)).event;
      expect(
        switch (lastEvent) {
        | Event.PayoutFinalized(_) => true
        | _ => false
        },
      )
      |> toEqual(true);
    });
  });
  Scenarios.runWithView("confirmed-income", viewModel => {
    let selectedVenture = viewModel |> ViewModel.selectedVenture;
    test("No unconfirmed income", () =>
      expect(selectedVenture.unconfirmedTxs |> List.size) |> toEqual(0)
    );
  });
};
