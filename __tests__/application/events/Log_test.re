open Jest;

open Expect;

module TestItem = {
  type t = string;
  let decode = Json.Decode.string;
  let encode = Json.Encode.string;
};

let () = {
  module TestLog = Log.Make(TestItem);
  let keyPair = Bitcoin.ECPair.makeRandom();
  describe("findNewItems", () =>
    test("preserve order", () => {
      let log =
        TestLog.make()
        |> TestLog.append(keyPair, "Watch out where")
        |> snd
        |> TestLog.append(keyPair, " the huskies go,")
        |> snd;
      let otherLog =
        log
        |> TestLog.append(keyPair, " and don't you")
        |> snd
        |> TestLog.append(keyPair, " eat")
        |> snd;
      let anotherLog =
        log
        |> TestLog.append(keyPair, " eat")
        |> snd
        |> TestLog.append(keyPair, " that yellow snow")
        |> snd;
      let result =
        log
        |> TestLog.findNewItems([otherLog, anotherLog])
        |> List.fold_left(
             (state, {event}: TestLog.item) => state ++ event,
             "",
           );
      expect(result) |> toEqual(" and don't you eat that yellow snow");
    })
  );
};
