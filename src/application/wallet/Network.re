open WalletTypes;

module Make = (Client: NetworkClient) => {
  let getUTXOs = addresses => {
    let addressStrings =
      addresses |> List.map((a: AccountKeyChain.Address.t) => a.address);
    Client.getUTXOs(addressStrings);
  };
};

module Testnet =
  Make(
    (
      val BitcoindClient.make(
            {
              bitcoindUrl: "http://localhost:18322",
              rpcUser: "bitcoin",
              rpcPassword: "bitcoin"
            }: BitcoindClient.config
          )
    )
  );
