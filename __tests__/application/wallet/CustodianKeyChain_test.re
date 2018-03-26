open Jest;

open Expect;

let () =
  describe("hashCode", () => {
    test("hashes an empty string", () =>
      expect(Utils.hashCode("")) |> toBe(0)
    );
    test("hashes a string", () =>
      expect(Utils.hashCode("frank")) |> toBe(97692050)
    );
  });
