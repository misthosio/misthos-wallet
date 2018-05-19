let feesUrl = "https://bitcoinfees.earn.com/api/v1/fees/recommended";

type recomendedFees = {
  fastestFee: BTC.t,
  halfHourFee: BTC.t,
  hourFee: BTC.t,
};

let fetchFees = () =>
  Js.Promise.(
    Fetch.fetch(feesUrl)
    |> then_(Fetch.Response.json)
    |> then_(res =>
         Json.Decode.{
           fastestFee:
             res
             |> field("fastestFee", Utils.decodeFloat)
             |> BTC.fromSatoshisFloat,
           halfHourFee:
             res
             |> field("halfHourFee", Utils.decodeFloat)
             |> BTC.fromSatoshisFloat,
           hourFee:
             res
             |> field("hourFee", Utils.decodeFloat)
             |> BTC.fromSatoshisFloat,
         }
         |> resolve
       )
  );
