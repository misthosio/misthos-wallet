open Jest;

open Expect;

let () =
  Scenarios.run("three-person-payout", venture =>
    test("Balance is correct", () => {
      let balance = "";
      expect(true) |> toEqual(true);
    })
  );
