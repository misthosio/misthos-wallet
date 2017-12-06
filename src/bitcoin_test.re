open Jest;

open Expect;

open Bitcoin;

let () = {
  describe(
    "ECPair",
    () =>
      test(
        "fromWIF",
        () => {
          let wif = "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn";
          let address = "1BgGZ9tcN4rm9KBzDn7KprQz87SZ26SAMH";
          let keyPair = ECPair.fromWIF(wif, [||]);
          expect(keyPair |> ECPair.toWIF) |> toBe(wif);
          expect(keyPair |> ECPair.getAddress) |> toBe(address)
        }
      )
  );
  describe("Networks", () => test("bitcoin", () => expect(Networks.bitcoin##bech32) |> toBe("bc")))
};
