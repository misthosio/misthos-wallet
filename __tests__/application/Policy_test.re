open Jest;

open Expect;

open PrimitiveTypes;

let () = {
  describe("Unanimous", () => {
    test("fulfilled", () => {
      let p = Policy.absolute;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~endorsed=["a" |> UserId.fromString, "b" |> UserId.fromString],
           ),
      )
      |> toBe(true);
    });
    test("not fulfilled", () => {
      let p = Policy.absolute;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~endorsed=["a" |> UserId.fromString, "c" |> UserId.fromString],
           ),
      )
      |> toBe(false);
    });
  });
  describe("UnanimousMinusOne", () => {
    test("fulfilled", () => {
      let p = Policy.absoluteMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~endorsed=["a" |> UserId.fromString, "b" |> UserId.fromString],
           ),
      )
      |> toBe(true);
    });
    test("fullfilled with minus 1 votes", () => {
      let p = Policy.absoluteMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=["a" |> UserId.fromString, "b" |> UserId.fromString],
             ~endorsed=["a" |> UserId.fromString, "c" |> UserId.fromString],
           ),
      )
      |> toBe(true);
    });
    test("not fullfilled", () => {
      let p = Policy.absoluteMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligable=[
               "a" |> UserId.fromString,
               "b" |> UserId.fromString,
               "c" |> UserId.fromString,
             ],
             ~endorsed=["a" |> UserId.fromString],
           ),
      )
      |> toBe(false);
    });
  });
};
