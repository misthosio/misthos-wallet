open PrimitiveTypes;

type t = {custodianKeyChains: list((userId, CustodianKeyChain.public))};

let make = custodianKeyChains => {
  custodianKeyChains: custodianKeyChains
  /* |> List.sort((chainA, chainB) => */
  /*      compare( */
  /*        chainA */
  /*        |> snd */
  /*        |> CustodianKeyChain.getHDNode */
  /*        |> Bitcoin.HDNode.getPublicKeyBuffer */
  /*        |> Utils.bufToHex, */
  /*        chainB */
  /*        |> snd */
  /*        |> CustodianKeyChain.getHDNode */
  /*        |> Bitcoin.HDNode.getPublicKeyBuffer */
  /*        |> Utils.bufToHex */
  /*      ) */
  /*    ) */
};

let custodianKeyChains = keyChain => keyChain.custodianKeyChains;

let encode = keyChain =>
  Json.Encode.(
    object_([
      (
        "custodianKeyChains",
        list(
          pair(UserId.encode, CustodianKeyChain.encode),
          keyChain.custodianKeyChains
        )
      )
    ])
  );

let decode = raw =>
  Json.Decode.{
    custodianKeyChains:
      raw
      |> field(
           "custodianKeyChains",
           list(pair(UserId.decode, CustodianKeyChain.decode))
         )
  };
