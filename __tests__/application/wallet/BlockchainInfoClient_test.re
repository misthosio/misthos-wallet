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
    testPromise(~timeout=50000, "getTransactionHex", () =>
      Js.Promise.(
        BlockchainInfoClient.getTransactionHex(
          {subdomain: "", network: Bitcoin.Networks.bitcoin},
          [|
            "b6f6991d03df0e2e04dafffcd6bc418aac66049e2cd74b80f14ac86db1e3f0da",
          |],
        )
        |> then_(res =>
             expect(res)
             |> toEqual([|
                  (
                    "b6f6991d03df0e2e04dafffcd6bc418aac66049e2cd74b80f14ac86db1e3f0da",
                    "010000000101820e2169131a77976cf204ce28685e49a6d2278861c33b6241ba3ae3e0a49f020000008b48304502210098a2851420e4daba656fd79cb60cb565bd7218b6b117fda9a512ffbf17f8f178022005c61f31fef3ce3f906eb672e05b65f506045a65a80431b5eaf28e0999266993014104f0f86fa57c424deb160d0fc7693f13fce5ed6542c29483c51953e4fa87ebf247487ed79b1ddcf3de66b182217fcaf3fcef3fcb44737eb93b1fcb8927ebecea26ffffffff02805cd705000000001976a91429d6a3540acfa0a950bef2bfdc75cd51c24390fd88ac80841e00000000001976a91417b5038a413f5c5ee288caa64cfab35a0c01914e88ac00000000",
                  ),
                |])
             |> resolve
           )
      )
    );
  });
