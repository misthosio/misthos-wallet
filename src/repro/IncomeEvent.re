type t = {
  address: string,
  coordinates: Address.Coordinates.t,
  txId: string,
  txOutputN: int,
  amount: BTC.t,
};

let make = (~txOutputN, ~coordinates, ~address, ~txId, ~amount) => {
  coordinates,
  address,
  txId,
  txOutputN,
  amount,
};

let encode = event =>
  Json.Encode.(
    object_([
      ("type", string("IncomeDetected")),
      ("address", string(event.address)),
      ("txId", string(event.txId)),
      ("txOutputN", int(event.txOutputN)),
      ("coordinates", Address.Coordinates.encode(event.coordinates)),
      ("amount", BTC.encode(event.amount)),
    ])
  );

let decode = raw =>
  Json.Decode.{
    address: raw |> field("address", string),
    txId: raw |> field("txId", string),
    amount: raw |> field("amount", BTC.decode),
    txOutputN: raw |> field("txOutputN", int),
    coordinates: raw |> field("coordinates", Address.Coordinates.decode),
  };
