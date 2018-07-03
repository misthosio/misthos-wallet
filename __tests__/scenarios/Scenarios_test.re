open Belt;

open Jest;
open Expect;

let () =
  /* Scenarios.run( */
  /*   ~checkIntegrity=true, */
  /*   "three-person-payout", */
  /*   (_venture, newItems) => { */
  /*     test("There are 2 new Items", () => */
  /*       expect(newItems |. Array.length) |> toEqual(2) */
  /*     ); */
  /*     test("Payout is finalized", () => { */
  /*       let lastEvent = (newItems |. Array.getExn(1)).event; */
  /*       expect( */
  /*         switch (lastEvent) { */
  /*         | Event.PayoutFinalized(_) => true */
  /*         | _ => false */
  /*         }, */
  /*       ) */
  /*       |> toEqual(true); */
  /*     }); */
  /*   }, */
  /* ); */
  /* Scenarios.run("four-person-payout", (_venture, newItems) => { */
  /*   test("There are 2 new Items", () => */
  /*     expect(newItems |. Array.length) |> toEqual(2) */
  /*   ); */
  /*   test("Payout is finalized", () => { */
  /*     let lastEvent = (newItems |. Array.getExn(1)).event; */
  /*     expect( */
  /*       switch (lastEvent) { */
  /*       | Event.PayoutFinalized(_) => true */
  /*       | _ => false */
  /*       }, */
  /*     ) */
  /*     |> toEqual(true); */
  /*   }); */
  /* }); */
  Scenarios.runWithView("addresses-details", viewModel => {
    let addressesView = viewModel |> ViewModel.viewAddressesModal;
    test("currentUtxos", () => {
      let info = addressesView.infos |. List.getExn(1);
      let details = addressesView.addressDetails(info);
      expect((info.address, details.currentUtxos |> List.length))
      |> toEqual(("2N59RD59Ddcpxemt7FtokEGGqi8kevujBKF", 2));
    });
  });
