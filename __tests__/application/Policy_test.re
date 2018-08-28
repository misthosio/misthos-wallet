open Jest;

open Expect;

open Belt;

open PrimitiveTypes;

let () = {
  describe("Unanimous", () => {
    test("fulfilled", () => {
      let p = Policy.unanimous;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true);
    });
    test("not fulfilled", () => {
      let p = Policy.unanimous;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "c" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(false);
    });
    test("at least one eligible", () => {
      let p = Policy.unanimous;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=UserId.emptySet,
             ~endorsed=UserId.emptySet,
           ),
      )
      |> toBe(false);
    });
    describe("canBeFullfilled", () => {
      test("with one rejection", () => {
        let p = Policy.unanimous;
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString, "c" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(false);
      });
      test("with zero rejections", () => {
        let p = Policy.unanimous;
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=UserId.emptySet,
             ),
        )
        |> toBe(true);
      });
    });
  });
  describe("UnanimousMinusOne", () => {
    test("fulfilled", () => {
      let p = Policy.unanimousMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true);
    });
    test("fullfilled with minus 1 votes", () => {
      let p = Policy.unanimousMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "c" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true);
    });
    test("not fullfilled", () => {
      let p = Policy.unanimousMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|
                 "a" |> UserId.fromString,
                 "b" |> UserId.fromString,
                 "c" |> UserId.fromString,
               |]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString|] |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(false);
    });
    test("at least one eligible", () => {
      let p = Policy.unanimousMinusOne;
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=UserId.emptySet,
             ~endorsed=UserId.emptySet,
           ),
      )
      |> toBe(false);
    });
    describe("canBeFullfilled", () => {
      test("with two rejections", () => {
        let p = Policy.unanimousMinusOne;
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|
                   "a" |> UserId.fromString,
                   "b" |> UserId.fromString,
                   "c" |> UserId.fromString,
                 |]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(false);
      });
      test("with one rejection", () => {
        let p = Policy.unanimousMinusOne;
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(true);
      });
      test("with zero rejections", () => {
        let p = Policy.unanimous;
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=UserId.emptySet,
             ),
        )
        |> toBe(true);
      });
    });
  });
  describe("Percentage", () => {
    let p = Policy.percentage(51);
    test("fulfilled", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|
                 "a" |> UserId.fromString,
                 "b" |> UserId.fromString,
                 "c" |> UserId.fromString,
               |]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true)
    );
    test("not fulfilled", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|
                 "a" |> UserId.fromString,
                 "b" |> UserId.fromString,
                 "c" |> UserId.fromString,
                 "d" |> UserId.fromString,
               |]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(false)
    );
    test("at least one eligible", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=UserId.emptySet,
             ~endorsed=UserId.emptySet,
           ),
      )
      |> toBe(false)
    );
    describe("canBeFullfilled", () => {
      test("with one rejection and 2 eligible", () =>
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(false)
      );
      test("with one rejection and 3 eligible", () =>
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|
                   "a" |> UserId.fromString,
                   "b" |> UserId.fromString,
                   "c" |> UserId.fromString,
                 |]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(true)
      );
    });
  });
  describe("AtLeast", () => {
    let p = Policy.atLeast(2);
    test("fulfilled", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true)
    );
    test("fullfilled with n votes", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|
                 "a" |> UserId.fromString,
                 "b" |> UserId.fromString,
                 "c" |> UserId.fromString,
               |]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
               |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(true)
    );
    test("not fullfilled", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=
               [|
                 "a" |> UserId.fromString,
                 "b" |> UserId.fromString,
                 "c" |> UserId.fromString,
                 "d" |> UserId.fromString,
               |]
               |> Set.mergeMany(UserId.emptySet),
             ~endorsed=
               [|"a" |> UserId.fromString|] |> Set.mergeMany(UserId.emptySet),
           ),
      )
      |> toBe(false)
    );
    test("at least one eligible", () =>
      expect(
        p
        |> Policy.fulfilled(
             ~eligible=UserId.emptySet,
             ~endorsed=UserId.emptySet,
           ),
      )
      |> toBe(false)
    );
    describe("canBeFullfilled", () => {
      test("with all but n - 1 rejections", () =>
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|
                   "a" |> UserId.fromString,
                   "b" |> UserId.fromString,
                   "c" |> UserId.fromString,
                   "d" |> UserId.fromString,
                 |]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|
                   "a" |> UserId.fromString,
                   "b" |> UserId.fromString,
                   "c" |> UserId.fromString,
                 |]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(false)
      );
      test("with all n but rejections", () =>
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|
                   "a" |> UserId.fromString,
                   "b" |> UserId.fromString,
                   "c" |> UserId.fromString,
                 |]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=
                 [|"a" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
             ),
        )
        |> toBe(true)
      );
      test("with zero rejections", () =>
        expect(
          p
          |> Policy.canBeFulfilled(
               ~eligible=
                 [|"a" |> UserId.fromString, "b" |> UserId.fromString|]
                 |> Set.mergeMany(UserId.emptySet),
               ~rejected=UserId.emptySet,
             ),
        )
        |> toBe(true)
      );
    });
  });
};
