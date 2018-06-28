open Jest;
open Expect;

let encode = (a, b) =>
  Json.Encode.(
    object_([
      ("a", nullable(int, a)),
      ("b", array(nullable(string), [|b|])),
    ])
  );
let () =
  describe("HashHelper.pruneNullKeys", () => {
    test("Prunes null values", () =>
      expect(
        encode(None, Some("bla"))
        |> HashHelper.pruneNullKeys
        |> Json.stringify,
      )
      |> toEqual({js|{"b":["bla"]}|js})
    );
    test("Does not change arrays", () =>
      expect(
        encode(Some(1), None) |> HashHelper.pruneNullKeys |> Json.stringify,
      )
      |> toEqual({js|{"a":1,"b":[null]}|js})
    );
  });
