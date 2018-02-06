open Jest;

open Expect;

open PrimitiveTypes;

let () =
  describe("Policy", () => {
    test("fulfilled", () => {
      let p = Policy.absolute;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~approved=["a" |> UserId.fromString, "b" |> UserId.fromString]
           )
      )
      |> toBe(true);
    });
    test("not fulfilled", () => {
      let p = Policy.absolute;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~approved=["a" |> UserId.fromString, "c" |> UserId.fromString]
           )
      )
      |> toBe(false);
    });
  });
