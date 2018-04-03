open WalletTypes;

type txInput = {
  txId: string,
  txOutputN: int,
  address: string,
  value: BTC.t,
  nCoSigners: int,
  confirmations: int,
  coordinates: AddressCoordinates.t
};

module Make = (Client: NetworkClient) => {
  let network = Client.network;
  let getTransactionInputs = (coordinates, accountKeyChains) => {
    let addresses =
      coordinates
      |> List.map(c => {
           let address = AccountKeyChain.find(c, accountKeyChains);
           (address.address, (c, address));
         });
    Js.Promise.(
      Client.getUTXOs(addresses |> List.map(a => fst(a)))
      |> then_(utxos =>
           utxos
           |> List.map(
                ({txId, txOutputN, address, amount, confirmations}: utxo) =>
                {
                  txId,
                  txOutputN,
                  address,
                  confirmations,
                  nCoSigners: snd(addresses |> List.assoc(address)).nCoSigners,
                  value: amount,
                  coordinates: addresses |> List.assoc(address) |> fst
                }
              )
           |> resolve
         )
    );
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
