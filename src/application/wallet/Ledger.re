open Belt;

open PrimitiveTypes;
open WalletTypes;

module L = LedgerJS;
module B = Bitcoin;

let getHDNode = (path, network, ledger) =>
  Js.Promise.(
    ledger
    |. L.getWalletPublicKey(path)
    |> then_(pubKey =>
         B.HDNode.fromPublicKey(
           ~publicKey=
             Utils.bufFromHex(pubKey##publicKey)
             |. B.ECPair.fromPublicKey({
                  "network": network |> Network.bitcoinNetwork,
                })
             |> B.ECPair.getPublicKey,
           ~chainCode=Utils.bufFromHex(pubKey##chainCode),
           network |> Network.bitcoinNetwork,
         )
         |> resolve
       )
  );

let misthosPurposeNode =
  getHDNode(CustodianKeyChain.misthosWalletPurposePath, Network.Mainnet);

let pathToBip45Root = CustodianKeyChain.makePathToBip45Root;

let getSigningPathAndPubKey =
    (ventureId, misthosPurposeNode, keyChain, coordinates) => {
  let path =
    misthosPurposeNode
    |> pathToBip45Root(
         ~ventureId,
         ~accountIdx=coordinates |> Address.Coordinates.accountIdx,
         ~keyChainIdx=keyChain |> CustodianKeyChain.keyChainIdx,
       );
  (
    path
    ++ "/"
    ++ string_of_int(
         coordinates |> Address.Coordinates.coSignerIdx |> CoSignerIndex.toInt,
       )
    ++ "/"
    ++ string_of_int(
         coordinates |> Address.Coordinates.chainIdx |> ChainIndex.toInt,
       )
    ++ "/"
    ++ string_of_int(
         coordinates |> Address.Coordinates.addressIdx |> AddressIndex.toInt,
       ),
    keyChain
    |> CustodianKeyChain.getPublicKey(
         coordinates |> Address.Coordinates.coSignerIdx,
         coordinates |> Address.Coordinates.chainIdx,
         coordinates |> Address.Coordinates.addressIdx,
       ),
  );
};

type result =
  | Ok(CustodianKeyChain.public)
  | WrongDevice
  | Error(LedgerJS.error);
let getCustodianKeyChain =
    (~network, ~ventureId, ~ledgerId, ~accountIdx, ~keyChainIdx) =>
  Js.Promise.(
    L.createTransport()
    |> then_(transport => {
         let btc = L.btc(transport);

         all2((resolve(btc), btc |> misthosPurposeNode));
       })
    |> then_(((btc, misthosPurposeNode)) => {
         let path =
           misthosPurposeNode
           |> pathToBip45Root(~ventureId, ~accountIdx, ~keyChainIdx);
         all2((
           misthosPurposeNode |> B.Address.fromHDNode |> resolve,
           btc |> getHDNode(path, network),
         ));
       })
    |> then_(((hardwareId, hdNode)) =>
         switch (ledgerId) {
         | Some(ledgerId) when ledgerId != hardwareId =>
           WrongDevice |> resolve
         | _ =>
           hdNode
           |> CustodianKeyChain.fromHardwareNode(
                ~hardwareId,
                ~accountIdx,
                ~keyChainIdx,
              )
           |. Ok
           |> resolve
         }
       )
    |> catch(error => error |> L.decodeError |. Error |> resolve)
  );

let dummyPath = "0'";
let dummyPubKey = "DUMMY";
type signResult =
  | Signatures(array(option((string, string))))
  | WrongDevice
  | Error(LedgerJS.error);
let signPayout =
    (
      ventureId,
      userId,
      ledgerId,
      {usedInputs, txHex}: PayoutTransaction.t,
      inputTxHexs,
      accountKeyChains,
    ) => {
  let txWrapper = TxWrapper.make(txHex);
  Js.Promise.(
    L.createTransport()
    |> then_(transport => {
         let btc = L.btc(transport);
         all2((resolve(btc), btc |> misthosPurposeNode));
       })
    |> then_(((btc, misthosPurposeNode)) => {
         let hardwareId = misthosPurposeNode |> B.Address.fromHDNode;
         if (hardwareId != ledgerId) {
           WrongDevice |> resolve;
         } else {
           let infos =
             usedInputs
             |. Array.zip(
                  inputTxHexs |. Array.map(L.splitTransaction(btc)),
                )
             |. Array.mapWithIndexU(
                  (.
                    idx,
                    ({txOutputN, coordinates}: Network.txInput, txInfo),
                  ) => {
                  let address: Address.t =
                    accountKeyChains |> Address.find(coordinates);
                  let accountKeyChain =
                    accountKeyChains
                    |> AccountKeyChain.Collection.lookup(
                         coordinates |> Address.Coordinates.accountIdx,
                         coordinates |> Address.Coordinates.keyChainIdent,
                       );
                  let pathAndPubKey =
                    switch (
                      accountKeyChain.custodianKeyChains
                      |. List.getAssoc(userId, UserId.eq)
                    ) {
                    | Some(keyChain)
                        when
                          keyChain
                          |> CustodianKeyChain.hardwareId
                          |> Js.Option.isSome =>
                      getSigningPathAndPubKey(
                        ventureId,
                        misthosPurposeNode,
                        keyChain,
                        coordinates,
                      )
                    | _ => (dummyPath, dummyPubKey)
                    };
                  (
                    (
                      txInfo,
                      txOutputN,
                      address.witnessScript,
                      (txWrapper.inputs |. Array.getExn(idx)).sequence,
                    ),
                    pathAndPubKey,
                  );
                });
           let (inputInfos, pathAndPubKeys) = infos |> Array.unzip;
           let (paths, pubKeys) = pathAndPubKeys |> Array.unzip;
           let outputScriptHex =
             txHex
             |> L.splitTransaction(btc)
             |> L.serializeTransactionOutputs(btc);
           btc
           |. L.signP2SHTransaction(
                inputInfos,
                paths,
                outputScriptHex |> Utils.bufToHex,
              )
           |> then_(signatures =>
                signatures
                |> Array.zip(pubKeys)
                |. Array.mapU((. (pubKey, signature)) =>
                     pubKey == dummyPubKey ? None : Some((pubKey, signature))
                   )
                |. Signatures
                |> resolve
              )
           |> catch(error => error |> L.decodeError |. Error |> resolve);
         };
       })
  );
};
