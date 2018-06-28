open Belt;

open Jest;
open Expect;

let () =
  Scenarios.run("three-person-payout", venture =>
    test("Last event is PayoutFinalized", () => {
      let items = venture |> Venture.getEventLog |> EventLog.items;
      let lastEvent = (items |. Array.getExn(48)).event;
      expect(
        switch (lastEvent) {
        | Event.PayoutFinalized(_) => true
        | _ => false
        },
      )
      |> toEqual(true);
    })
  );
