open Jest;

open Expect;

open Project;

let () =
  describe("Serialization", () =>
    test("true", ()
      =>
        expect(true) |> toBe(true)
      )
      /* test("empty", () => */
      /*   expect(Project.Decode.index("[]")) |> toEqual([]) */
      /* ); */
      /* test("Decode items", () => */
      /*   expect( */
      /*     Project.Decode.index( */
      /*       {| [{"name": "projectA", "id": "A"},{"name": "projectB", "id": "B"}] |} */
      /*     ) */
      /*   ) */
      /*   |> toEqual([{name: "projectA", id: "A"}, {name: "projectB", id: "B"}]) */
      /* ); */
      /* test("Encode items", () => */
      /*   expect(Project.Encode.index([{name: "projectA", id: "A"}])) */
      /* ); */
      /*   |> toEqual({|[{"name":"projectA","id":"A"}]|}) */
  );
