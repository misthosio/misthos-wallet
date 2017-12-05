open Jest;

open Expect;

open Bitcoin;

let () =
  describe(
    "ECPair",
    () =>
      test(
        "makeRandom",
        () => {
          let keyPair = ECPair.makeRandom();
          Js.log(keyPair);
          expect(ECPair.hello) |> toBe("sahtoen")
        }
      )
  );
