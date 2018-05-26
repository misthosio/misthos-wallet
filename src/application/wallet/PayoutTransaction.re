module B = Bitcoin;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

module Fee = TransactionFee;

type input = Network.txInput;

let misthosFeePercent = 1.49;

type t = {
  txHex: string,
  usedInputs: array(input),
  misthosFeeAddress: string,
  changeAddress: option(Address.t),
};

let txInputForChangeAddress = (~txId, network, {changeAddress, txHex}) =>
  changeAddress
  |> Utils.mapOption((address: Address.t) => {
       let tx = B.Transaction.fromHex(txHex);
       let (idx, value) =
         tx##outs
         |> Array.to_list
         |> List.mapi((i, out) =>
              B.Address.fromOutputScript(
                out##script,
                Bitcoin.Networks.testnet,
              )
              == address.displayAddress ?
                Some((i, BTC.fromSatoshisFloat(out##value))) : None
            )
         |> List.find(Js.Option.isSome)
         |> Js.Option.getExn;
       Network.{
         txId,
         txOutputN: idx,
         value,
         nCoSigners: address.nCoSigners,
         nPubKeys: address.nPubKeys,
         address: address.displayAddress,
       };
     });

let encode = payout =>
  Json.Encode.(
    object_([
      ("txHex", string(payout.txHex)),
      ("usedInputs", array(Network.encodeInput, payout.usedInputs)),
      ("misthosFeeAddress", string(payout.misthosFeeAddress)),
      ("changeAddress", nullable(Address.encode, payout.changeAddress)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    txHex: raw |> field("txHex", string),
    usedInputs: raw |> field("usedInputs", array(Network.decodeInput)),
    misthosFeeAddress: raw |> field("misthosFeeAddress", string),
    changeAddress: raw |> field("changeAddress", optional(Address.decode)),
  };
