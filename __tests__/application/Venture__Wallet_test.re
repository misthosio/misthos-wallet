/* Helpers.enableHttpRequests(); */
open Jest;

open Expect;

open PrimitiveTypes;

open WalletTypes;

open Bitcoin;

open Event;

module Wallet = Venture__Wallet;

let () =
  describe("preparePayoutTx", () => {
    let (keyA, keyB) = (
      ECPair.fromWIFWithNetwork(
        "cUVTgxrs44T7zVon5dSDicBkBRjyfLwL7RF1RvR7n94ar3HEaLs1",
        Networks.testnet
      ),
      ECPair.fromWIFWithNetwork(
        "cPfdeLvhwvAVRRM5wiEWopWviGG65gbxQCHdtFL56PYUJXsTYixf",
        Networks.testnet
      )
    );
    let chainCode =
      "c8bce5e6dac6f931af17863878cce2ca3b704c61b3d775fe56881cc8ff3ab1cb"
      |> Utils.bufFromHex;
    let (masterA, masterB) = (
      HDNode.make(keyA, chainCode),
      HDNode.make(keyB, chainCode)
    );
    let ventureId = VentureId.fromString("test");
    let accountIdx = AccountIndex.default;
    let keyChainIdx = CustodianKeyChainIndex.first;
    let (cKeyChainA, cKeyChainB) = (
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterA
      )
      |> CustodianKeyChain.toPublicKeyChain,
      CustodianKeyChain.make(
        ~ventureId,
        ~accountIdx,
        ~keyChainIdx,
        ~masterKeyChain=masterB
      )
      |> CustodianKeyChain.toPublicKeyChain
    );
    test("", () =>
      expect(true) |> toBe(true)
    );
  });
/* testPromise( */
/*   ~timeout=10000, */
/*   "wip", */
/*   () => { */
/*     let accountKeyChain = */
/*       AccountKeyChain.make( */
/*         1, */
/*         [ */
/*           (UserId.fromString("custodianA"), cKeyChainA), */
/*           (UserId.fromString("custodianB"), cKeyChainB) */
/*         ] */
/*       ); */
/*     let wallet = */
/*       Wallet.make() */
/*       |> Wallet.apply( */
/*            AccountKeyChainUpdated( */
/*              AccountKeyChainUpdated.make( */
/*                ~accountIdx, */
/*                ~keyChainIdx=AccountKeyChainIndex.first, */
/*                ~keyChain=accountKeyChain */
/*              ) */
/*            ) */
/*          ); */
/*     let address1 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx); */
/*     Js.log(address1); */
/*     let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address1)); */
/*     let address2 = wallet |> Wallet.exposeNextIncomeAddress(accountIdx); */
/*     Js.log(address2); */
/*     let wallet = wallet |> Wallet.apply(IncomeAddressExposed(address2)); */
/*     Js.Promise.( */
/*       Helpers.faucet([ */
/*         (address1.address, BTC.fromSatoshis(10000L)), */
/*         (address2.address, BTC.fromSatoshis(10000L)) */
/*       ]) */
/*       |> then_((_) => wallet |> Wallet.preparePayoutTx(accountIdx, [], 0)) */
/*       |> then_(utxos => { */
/*            Js.log(utxos); */
/*            expect(true) |> toEqual(true) |> resolve; */
/*          }) */
/*     ); */
/*   } */
/* ); */
