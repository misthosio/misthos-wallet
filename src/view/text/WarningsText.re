include ViewCommon;

let testnet =
  [|
    "Warning! This is the testnet version of Misthos.  Get Testnet coins "
    |> text,
    <a target="_blank" href="https://testnet.manu.backend.hamburg/faucet">
      ("here" |> text)
    </a>,
    " and get notified of our Mainnet release " |> text,
    <a
      target="_blank"
      href="https://misthos.us17.list-manage.com/subscribe/post?u=1696fffacc1f8609ca14818f3&id=e0d336cc53">
      ("here" |> text)
    </a>,
    "." |> text,
  |]
  |> ReasonReact.array;

let atRiskFunds = ventureId => {
  let route = Router.Config.(Venture(ventureId, CreatePayout));
  [|
    "Some of your addresses contain at-risk funds. Please " |> text,
    <a
      href=(route |> Router.Config.routeToUrl)
      onClick=(Router.clickToRoute(route))>
      ("Make a payout" |> text)
    </a>,
    " To avoid your funds becoming inaccessible." |> text,
  |]
  |> ReasonReact.array;
};

let partnerRemovalRisk = "ALERT: This user is currently critical to access certain funds. By removing this user, you accept that these funds will become temporarily or permanently inaccessible. Are you sure you want to endorse this removal?";
