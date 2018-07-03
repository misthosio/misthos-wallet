open Belt;

open Jest;
open Expect;

let () = {
  Scenarios.run(
    ~checkIntegrity=true,
    "three-person-payout",
    (_venture, newItems) => {
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
    },
  );
  Scenarios.run("four-person-payout", (_venture, newItems) => {
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
};
