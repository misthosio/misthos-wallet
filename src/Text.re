include ViewCommon;
module Warnings = {
  let testnet =
    [|
      "Warning! This is the testnet version of Misthos.  Get Testnet coins "
      |> text,
      <a href="https://testnet.manu.backend.hamburg/faucet">
        ("here" |> text)
      </a>,
      " and get notified of our Mainnet release " |> text,
      <a
        href="https://misthos.us17.list-manage.com/subscribe/post?u=1696fffacc1f8609ca14818f3&id=e0d336cc53">
        ("here" |> text)
      </a>,
      "." |> text,
    |]
    |> ReasonReact.array;
};
