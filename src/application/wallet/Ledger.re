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

let pathToBip45Root =
    (~ventureId, ~misthosPurposeNode, ~accountIdx, ~keyChainIdx) =>
  misthosPurposeNode
  |> CustodianKeyChain.makePathToBip45Root(
       ~ventureId,
       ~accountIdx,
       ~keyChainIdx,
     );

let getSigningPathAndPubKey =
    (ventureId, misthosPurposeNode, keyChainIdx, coordinates, btc) => {
  let path =
    pathToBip45Root(
      ~ventureId,
      ~misthosPurposeNode,
      ~accountIdx=coordinates |> Address.Coordinates.accountIdx,
      ~keyChainIdx,
    );
  let pathToSigningNode =
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
       );

  Js.Promise.(
    btc
    |> getHDNode(pathToSigningNode, Network.Mainnet)
    |> then_(node =>
         (pathToSigningNode, node |> B.HDNode.getPublicKey |> Utils.bufToHex)
         |> resolve
       )
  );
};

type result =
  | Ok(CustodianKeyChain.public)
  | Error(LedgerJS.error);
let getCustodianKeyChain = (~network, ~ventureId, ~accountIdx, ~keyChainIdx) =>
  Js.Promise.(
    L.createTransport()
    |> then_(transport => {
         let btc = L.btc(transport);

         all2((resolve(btc), btc |> misthosPurposeNode));
       })
    |> then_(((btc, misthosPurposeNode)) => {
         let path =
           pathToBip45Root(
             ~ventureId,
             ~misthosPurposeNode,
             ~accountIdx,
             ~keyChainIdx,
           );
         all2((
           misthosPurposeNode |> B.Address.fromHDNode |> resolve,
           btc |> getHDNode(path, network),
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

let dummyPath = "0'";
let dummyPubKey = "DUMMY";
let signPayout =
    (
      ventureId,
      userId,
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
    |> then_(((btc, misthosPurposeNode)) =>
         all2((
           resolve(btc),
           usedInputs
           |. Array.zip(inputTxHexs |. Array.map(L.splitTransaction(btc)))
           |. Array.mapWithIndexU(
                (. idx, ({txOutputN, coordinates}: Network.txInput, txInfo)) => {
                let address: Address.t =
                  accountKeyChains |> Address.find(coordinates);
                let accountKeyChain =
                  accountKeyChains
                  |> AccountKeyChain.Collection.lookup(
                       coordinates |> Address.Coordinates.accountIdx,
                       coordinates |> Address.Coordinates.keyChainIdent,
                     );
                let pathAndPubKeyPromise =
                  switch (
                    accountKeyChain.custodianKeyChains
                    |. List.getAssoc(userId, UserId.eq)
                  ) {
                  | Some(keyChain)
                      when
                        keyChain
                        |> CustodianKeyChain.hardwareId
                        |> Js.Option.isSome =>
                    btc
                    |> getSigningPathAndPubKey(
                         ventureId,
                         misthosPurposeNode,
                         keyChain |> CustodianKeyChain.keyChainIdx,
                         coordinates,
                       )
                  | _ => (dummyPath, dummyPubKey) |> resolve
                  };
                all2((
                  (
                    txInfo,
                    txOutputN,
                    address.witnessScript,
                    (txWrapper.inputs |. Array.getExn(idx)).sequence,
                  )
                  |> resolve,
                  pathAndPubKeyPromise,
                ));
              })
           |> all,
         ))
       )
    |> then_(((btc, infos)) => {
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
         |> then_(signatures => signatures |. Array.zip(pubKeys) |> resolve);
       })
  );
};
