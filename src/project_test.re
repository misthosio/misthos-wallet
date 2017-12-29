open Jest;

open Expect;

open Project;

let () =
  describe(
    "Serialization",
    () => {
      test("empty", () => expect(Decode.index("[]")) |> toEqual([]));
      test(
        "Decode items",
        () =>
          expect(Decode.index({| [{"name": "projectA"},{"name": "projectB"}] |}))
          |> toEqual([{name: "projectA"}, {name: "projectB"}])
      );
      test(
        "Encode items",
        () =>
          expect(Encode.index([{name: "projectA"}, {name: "projectB"}]))
          |> toEqual({|[{"name":"projectA"},{"name":"projectB"}]|})
      )
    }
  );
