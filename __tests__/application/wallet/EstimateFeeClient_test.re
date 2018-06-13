Helpers.enableHttpRequests();
open Jest;
open Expect;

let () =
  describe("fees", () =>
    testPromise("will fetch", () =>
      Js.Promise.(
        EstimateFeeClient.fetchFees()
        |> then_(({high, normal}: EstimateFeeClient.result) =>
             expect(high |> BTC.gt(normal)) |> toBe(true) |> resolve
           )
      )
    )
  );
