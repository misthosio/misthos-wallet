open Jest;

open Expect;

open WalletTypes;

let () =
  describe("build", () => {
    let inputs: Network.inputSet =
      [|
        (
          {
            txId: "d66c39a24f63d80c13e44cf1ce562618d1d0d92675118aa331e5367a7ddb9de7",
            txOutputN: 0,
            address: "2N3gWQwj2RrHaw7rWmbr1vKkzBnutSMp2LE",
            value: BTC.fromSatoshis(10000L),
            nCoSigners: 1,
            nPubKeys: 1,
            coordinates: (
              AccountIndex.first,
              "identifier",
              CoSignerIndex.first,
              ChainIndex.externalChain,
              AddressIndex.first,
            ),
          }: Network.txInput
        ),
        (
          {
            txId: "d66c39a24f63d80c13e44cf1ce562618d1d0d92675118aa331e5367a7ddb9de7",
            txOutputN: 1,
            nPubKeys: 1,
            address: "2N3CDv7U6xVYmNqdvNscKBWwUYky7SM6Wdq",
            value: BTC.fromSatoshis(5000L),
            nCoSigners: 1,
            coordinates: (
              AccountIndex.first,
              "identifier",
              CoSignerIndex.first,
              ChainIndex.externalChain,
              AddressIndex.first |> AddressIndex.next,
            ),
          }: Network.txInput
        ),
      |]
      |> Belt.Set.fromArray(~id=(module Network.TxInputCmp));
    let changeAddress: Address.t = {
      nCoSigners: 1,
      nPubKeys: 1,
      coordinates: (
        AccountIndex.first,
        "identifier",
        CoSignerIndex.first,
        ChainIndex.externalChain,
        AddressIndex.first,
      ),
      witnessScript: "51210358ebee38e86598266dc351dfec81c0bd98e3a90a4e93bff72003569f2b02d13351ae",
      redeemScript: "002027fa0596838478a59b5c0512acf480fdba510cd320def9e3d9e9d27a13b7e72f",
      address: "2N3gWQwj2RrHaw7rWmbr1vKkzBnutSMp2LE",
    };
    test("uses as many inputs as necessary", () => {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=Network.inputSet(),
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(10000L)),
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest,
        );
      let changeUsed = payoutTx.changeAddress |> Js.Option.isSome;
      expect((payoutTx.usedInputs |> Array.length, changeUsed))
      |> toEqual((2, true));
    });
    test("uses smallest possible input", () => {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=Network.inputSet(),
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(4000L)),
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest,
        );
      let changeUsed = payoutTx.changeAddress |> Js.Option.isSome;
      expect((payoutTx.usedInputs[0].txOutputN, changeUsed))
      |> toEqual((1, true));
    });
    test("doesn't use change address if not worth it", () => {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=Network.inputSet(),
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(9500L)),
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest,
        );
      let changeUsed = payoutTx.changeAddress |> Js.Option.isSome;
      expect((payoutTx.usedInputs[0].txOutputN, changeUsed))
      |> toEqual((0, false));
    });
    test("respects mandatory inputs", () => {
      let payoutTx =
        PayoutTransaction.build(
          ~mandatoryInputs=
            inputs
            |. Belt.Set.keepU((. input: Network.txInput) =>
                 input.txOutputN == 1
               ),
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(6000L)),
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest,
        );
      let changeUsed = payoutTx.changeAddress |> Js.Option.isSome;
      expect((payoutTx.usedInputs |> Array.length, changeUsed))
      |> toEqual((2, true));
    });
    test("raises when there aren't enough funds", () =>
      expectFn(
        () =>
          PayoutTransaction.build(
            ~mandatoryInputs=Network.inputSet(),
            ~allInputs=inputs,
            ~destinations=[
              (
                "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
                BTC.fromSatoshis(15000L),
              ),
            ],
            ~satsPerByte=BTC.fromSatoshis(1L),
            ~changeAddress,
            ~network=Network.Regtest,
          ),
        (),
      )
      |> toThrow
    );
    test("summary", () => {
      let summary =
        PayoutTransaction.build(
          ~mandatoryInputs=Network.inputSet(),
          ~allInputs=inputs,
          ~destinations=[
            ("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", BTC.fromSatoshis(9800L)),
          ],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~changeAddress,
          ~network=Network.Regtest,
        )
        |> PayoutTransaction.summary(Network.Regtest);
      expect(summary)
      |> toEqual(
           PayoutTransaction.{
             reserved: BTC.fromSatoshis(15000L),
             spentWithFees: BTC.fromSatoshis(10370L),
             misthosFee: BTC.fromSatoshis(285L),
             networkFee: BTC.fromSatoshis(285L),
           },
         );
    });
    describe("max", () => {
      let max =
        PayoutTransaction.max(
          ~allInputs=inputs,
          ~targetDestination="mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
          ~destinations=[],
          ~satsPerByte=BTC.fromSatoshis(1L),
          ~network=Network.Regtest,
        );
      test("sending max amount works", () => {
        let summary =
          PayoutTransaction.build(
            ~mandatoryInputs=Network.inputSet(),
            ~allInputs=inputs,
            ~destinations=[("mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU", max)],
            ~satsPerByte=BTC.fromSatoshis(1L),
            ~changeAddress,
            ~network=Network.Regtest,
          )
          |> PayoutTransaction.summary(Network.Regtest);
        expect(summary)
        |> toEqual(
             PayoutTransaction.{
               reserved: BTC.fromSatoshis(15000L),
               spentWithFees: BTC.fromSatoshis(15000L),
               misthosFee: BTC.fromSatoshis(415L),
               networkFee: BTC.fromSatoshis(285L),
             },
           );
      });
      test("Spending more than max will throw an exception", () =>
        expectFn(
          () =>
            PayoutTransaction.build(
              ~mandatoryInputs=Network.inputSet(),
              ~allInputs=inputs,
              ~destinations=[
                (
                  "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
                  max |> BTC.plus(BTC.fromSatoshis(50L)),
                ),
              ],
              ~satsPerByte=BTC.fromSatoshis(1L),
              ~changeAddress,
              ~network=Network.Regtest,
            ),
          (),
        )
        |> toThrow
      );
    });
  });
