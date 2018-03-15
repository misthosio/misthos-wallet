let enableHttpRequests = () => [%bs.raw
  {| global.XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest |}
];

let startBitcoind = () => {
  let dirname =
    switch [%node __dirname] {
    | Some(name) => name
    | None => raise(Not_found)
    };
  Js.log("Starting bitcoind");
  Js.log(
    Node.Child_process.execSync(
      dirname ++ "/start_bitcoind.sh",
      Node.Child_process.option(~encoding="utf8", ())
    )
  );
};

let stopBitcoind = () => {
  let dirname =
    switch [%node __dirname] {
    | Some(name) => name
    | None => raise(Not_found)
    };
  Js.log("Stopping bitcoind");
  Js.log(
    Node.Child_process.execSync(
      dirname ++ "/stop_bitcoind.sh",
      Node.Child_process.option(~encoding="utf8", ())
    )
  );
};
