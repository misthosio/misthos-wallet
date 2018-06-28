open Jest;

open Expect;

let () = {
  let oneSatPerByte = BTC.fromSatoshis(1L);
  describe("inputCost", () => {
    test("1 cosigner", () =>
      expect(
        TransactionFee.inputCost(1, 2, oneSatPerByte) |> BTC.toSatoshisFloat,
      )
      |> toEqual(113.)
    );
    test("2 cosigners", () =>
      expect(
        TransactionFee.inputCost(2, 3, oneSatPerByte) |> BTC.toSatoshisFloat,
      )
      |> toEqual(140.)
    );
  });
  describe("outputCost", () => {
    test("p2pkh output", () =>
      expect(
        TransactionFee.outputCost(
          "mgWUuj1J1N882jmqFxtDepEC73Rr22E9GU",
          oneSatPerByte,
          Bitcoin.Networks.testnet,
        ),
      )
      |> toEqual(BTC.fromSatoshis(34L))
    );
    test("p2sh output", () =>
      expect(
        TransactionFee.outputCost(
          "2N8qFbjFX4ZA1jTatE17kYZnS849NB9bN2T",
          oneSatPerByte,
          Bitcoin.Networks.testnet,
        ),
      )
      |> toEqual(BTC.fromSatoshis(32L))
    );
  });
};
