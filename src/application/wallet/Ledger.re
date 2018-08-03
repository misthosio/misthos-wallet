open Bitcoin;

module L = LedgerJS;

let getHDNode = (path, network, ledger) =>
  Js.Promise.(
    ledger
    |. L.getWalletPublicKey(path)
    |> then_(pubKey =>
         HDNode.fromPublicKey(
           ~publicKey=
             Utils.bufFromHex(pubKey##publicKey)
             |. ECPair.fromPublicKey({"network": network})
             |> ECPair.getPublicKey,
           ~chainCode=Utils.bufFromHex(pubKey##chainCode),
           network,
         )
         |> resolve
       )
  );

type result =
  | Ok(CustodianKeyChain.public)
  | Error(LedgerJS.error);
let getCustodianKeyChain = (~network, ~ventureId, ~accountIdx, ~keyChainIdx) =>
  Js.Promise.(
    L.createTransport()
    |> then_(transport => {
         let btc = L.btc(transport);
         all2((
           resolve(btc),
           getHDNode(
             CustodianKeyChain.misthosWalletPurposePath,
             Networks.bitcoin,
             btc,
           ),
         ));
       })
    |> then_(((btc, misthosPurposeNode)) => {
         let path =
           misthosPurposeNode
           |> CustodianKeyChain.makePathToBip45Root(
                ~ventureId,
                ~accountIdx,
                ~keyChainIdx,
              );
         all2((
           misthosPurposeNode |> Address.fromHDNode |> resolve,
           getHDNode(path, network, btc),
         ));
       })
    |> then_(((hardwareId, hdNode)) =>
         hdNode
         |> CustodianKeyChain.fromHardwareNode(
              ~hardwareId,
              ~accountIdx,
              ~keyChainIdx,
            )
         |. Ok
         |> resolve
       )
    |> catch(error => error |> L.decodeError |. Error |> resolve)
  );
