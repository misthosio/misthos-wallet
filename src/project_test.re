open Jest;

open Expect;

open Project;

let () =
  describe("Serialization", () => {
    test("empty", () =>
      expect(Decode.index("[]")) |> toEqual([])
    );
    test("Decode items", () =>
      expect(
        Decode.index(
          {| [{"name": "projectA", "id": "A"},{"name": "projectB", "id": "B"}] |}
        )
      )
      |> toEqual([{name: "projectA", id: "A"}, {name: "projectB", id: "B"}])
    );
    test("Encode items", () =>
      expect(Encode.index([{name: "projectA", id: "A"}]))
      |> toEqual({|[{"name":"projectA","id":"A"}]|})
    );
  });
