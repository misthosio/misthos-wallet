let estimateFeeUrl = "https://estimatefee.com/n/";

type result = {
  high: BTC.t,
  normal: BTC.t,
  economy: BTC.t,
};
let nConfsUrl = n => estimateFeeUrl ++ string_of_int(n);
let fetchNConfs = n =>
  Js.Promise.(
    Fetch.fetch(nConfsUrl(n))
    |> then_(Fetch.Response.json)
    |> then_(res => Json.Decode.float(res) |> BTC.fromFloat |> resolve)
  );
let fetchFees = () =>
  Js.Promise.(
    all3((fetchNConfs(2), fetchNConfs(6), fetchNConfs(12)))
    |> then_(((high, normal, economy)) =>
         {
           high: high |> BTC.dividedByRounded(1000.),
           normal: normal |> BTC.dividedByRounded(1000.),
           economy: economy |> BTC.dividedByRounded(1000.),
         }
         |> resolve
       )
  );
