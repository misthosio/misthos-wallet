include BigNumber;

type encoded = Js.Json.t;

let fromSatoshis = satoshis =>
  BigNumber.make(`Float(satoshis |> Int64.to_float));

let fromSatoshisFloat = satoshis => BigNumber.make(`Float(satoshis));

let toSatoshisFloat = btc =>
  btc
  |. BigNumber.integerValue(BigNumber.RoundingMode.ceil)
  |> BigNumber.toNumber;

let zero = BigNumber.make(`Float(0.));

let satoshisPerBTC = BigNumber.make(`String("1e8"));

let fromString = btcString =>
  BigNumber.make(`String(btcString))
  |> BigNumber.times(satoshisPerBTC)
  |. BigNumber.integerValue(BigNumber.RoundingMode.floor);

let format = btc =>
  btc |. BigNumber.dividedBy(satoshisPerBTC) |> BigNumber.toString;

let fromFloat = btcFloat =>
  BigNumber.make(`Float(btcFloat)) |> BigNumber.times(satoshisPerBTC);

let timesRounded = (btc, n) =>
  btc |. timesFloat(n) |. BigNumber.integerValue(BigNumber.RoundingMode.ceil);

let dividedByRounded = (btc, n) =>
  btc
  |. dividedByFloat(n)
  |. BigNumber.integerValue(BigNumber.RoundingMode.floor);

let encode = toJSON;

let decode = raw => BigNumber.make(`String(Json.Decode.string(raw)));
