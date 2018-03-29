open WalletTypes;

module Make = (Client: NetworkClient) => {
  let network = Client.network;
  let getUTXOs = addresses => {
    let addressStrings =
      addresses |> List.map((a: AccountKeyChain.Address.t) => a.address);
    Client.getUTXOs(addressStrings);
  };
};

module Regtest =
  Make(
    (
      val BitcoindClient.make(
            {
              bitcoindUrl: "http://localhost:18322",
              rpcUser: "bitcoin",
              rpcPassword: "bitcoin"
            }: BitcoindClient.config,
            Bitcoin.Networks.testnet
          )
    )
  );
