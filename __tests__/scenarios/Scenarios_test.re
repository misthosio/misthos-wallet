open Belt;

open Jest;
open Expect;

let () =
  Scenarios.run("three-person-payout", (_venture, newItems) =>
    test("Last event is PayoutFinalized", () => {
      let lastEvent = (newItems |. Array.getExn(1)).event;
      expect(
        switch (lastEvent) {
        | Event.PayoutFinalized(_) => true
        | _ => false
        },
      )
      |> toEqual(true);
    })
  );
