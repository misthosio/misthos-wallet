Helpers.enableHttpRequests();
open Jest;
open Expect;

let () =
  describe("fees", () =>
    testPromise("will fetch", () =>
      Js.Promise.(
        EstimateFeeClient.fetchFees()
        |> then_(({high, normal, economy}: EstimateFeeClient.result) => {
             Js.log3(
               high |> BTC.format,
               normal |> BTC.format,
               economy |> BTC.format,
             );
             expect(high |> BTC.gt(BTC.zero)) |> toBe(true) |> resolve;
           })
      )
    )
  );
