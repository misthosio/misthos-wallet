open Jest;

open Expect;

let () =
  describe("Policy", () => {
    test("fulfilled", () => {
      let p = Policy.absolute;
      expect(p |> Policy.fulfilled(~eligable=["a", "b"], ~approved=["a", "b"]))
      |> toBe(true);
    });
    test("not fulfilled", () => {
      let p = Policy.absolute;
      expect(p |> Policy.fulfilled(~eligable=["a", "b"], ~approved=["a", "c"]))
      |> toBe(false);
    });
  });
