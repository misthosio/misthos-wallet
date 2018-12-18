Helpers.enableHttpRequests();
open Jest;
open Expect;

let () =
  describe("fees", () =>
    testPromise("will fetch", () =>
      Js.Promise.(
        EstimateFeeClient.fetchFees()
        |> then_(({high, economy}: EstimateFeeClient.result) =>
             expect(
               high->(BTC.gt(economy))
               || high->(BTC.comparedTo(BTC.fromSatoshis(1L))) == 0,
             )
             |> toBe(true)
             |> resolve
           )
      )
    )
  );
