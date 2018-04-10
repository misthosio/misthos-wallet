Helpers.enableHttpRequests();

open Jest;

open Expect;

let () =
  describe("getUTXOs", () =>
    testPromise("get stuff", () =>
      Js.Promise.(
        BlockcypherClient.getUTXOs(
          {network: "main"},
          [
            "1Ez69SnzzmePmZX3WpEzMKTrcBF2gpNQ55",
            "1XPTgDRhN8RFnzniWCddobD9iKZatrvH4",
            "14rE7Jqy4a6P27qWCCsngkUfBxtevZhPHB",
            "1M8s2S5bgAzSSzVTeL7zruvMPLvzSkEAuv",
            "1GBwk2YJMDFqSVhTKygH8zUwV7jdoJhHHH"
          ]
        )
        |> then_(res =>
             expect(res |> List.length) |> toBeGreaterThan(500) |> resolve
           )
      )
    )
  );
