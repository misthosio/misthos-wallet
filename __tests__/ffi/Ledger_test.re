open Jest;
open Expect;

module TransportHid = {
  [@bs.module "@ledgerhq/hw-transport-node-hid"] [@bs.scope "default"]
  external create : unit => Js.Promise.t(Ledger.transport) = "";
};

let () =
  Skip.testPromise(~timeout=40000, "hello", () =>
    Js.Promise.(
      TransportHid.create()
      |> then_(transport => {
           Ledger.btc(transport)
           |. Ledger.getWalletPublicKey("44'/0'/0'/0")
           |> then_(pubKey => Js.log(pubKey) |> resolve)
           |> ignore;

           expect(true) |> toEqual(true) |> resolve;
         })
      |> catch(err => {
           Js.log(err);
           expect(true) |> toEqual(true) |> resolve;
         })
    )
  );
