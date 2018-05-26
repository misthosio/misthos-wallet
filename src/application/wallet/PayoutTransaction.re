module B = Bitcoin;

exception NotEnoughFunds;

exception NotEnoughSignatures;

exception NoSignaturesForInput;

type input = Network.txInput;

type t = {
  txHex: string,
  usedInputs: array(input),
  misthosFeeAddress: string,
  changeAddress: option(Address.t),
};

let txInputForChangeAddress = (~txId, {changeAddress}) =>
  changeAddress
  |> Utils.mapOption((_) =>
       Network.{
         txId,
         txOutputN: 1,
         value: BTC.zero,
         nCoSigners: 3,
         nPubKeys: 2,
         address: "public",
       }
     );

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
