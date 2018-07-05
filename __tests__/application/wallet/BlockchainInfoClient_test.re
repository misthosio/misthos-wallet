Helpers.enableHttpRequests();

open Jest;

open Expect;

let () =
  describe("BlockchainInfoClient", () => {
    testPromise(~timeout=50000, "getUTXOs", () =>
      Js.Promise.(
        BlockchainInfoClient.getUTXOs(
          BlockchainInfoClient.mainnetConfig,
          ["3D2oetdNuZUqQHPJmcMDDHYoqkyNVsFk9r"],
        )
        |> then_(res =>
             expect(res |> List.length) |> toBeGreaterThan(200) |> resolve
           )
      )
    );
    testPromise(~timeout=50000, "blockheight", () =>
      Js.Promise.(
        BlockchainInfoClient.getCurrentBlockHeight(
          {subdomain: "", network: Bitcoin.Networks.bitcoin},
          (),
        )
        |> then_(res => expect(res) |> toBeGreaterThan(530440) |> resolve)
      )
    );
  });
