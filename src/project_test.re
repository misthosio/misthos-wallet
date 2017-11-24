open Jest;

open Expect;

open Project;

let () =
  describe(
    "Serialization",
    () => {
      test("empty", () => expect(Serialization.projectsFromJson("[]")) |> toEqual([]));
      test(
        "items",
        () =>
          expect(Serialization.projectsFromJson({| [{"name": "projectA"},{"name": "projectB"}] |}))
          |> toEqual([{name: "projectA"}, {name: "projectB"}])
      )
    }
  );
