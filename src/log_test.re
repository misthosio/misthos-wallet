open Jest;

open Expect;

module TestPayload = {
  type data = {
    text: string,
    number: int
  };
  type t =
    | StringEvent(string)
    | IntEvent(int)
    | DataEvent(data);
  let decode = (payload, ~type_) =>
    switch type_ {
    | "StringEvent" => StringEvent(payload)
    | "IntEvent" => IntEvent(Json.parseOrRaise(payload) |> Json.Decode.int)
    | "DataEvent" =>
      let json = Json.parseOrRaise(payload);
      DataEvent(
        Json.Decode.{text: json |> field("text", string), number: json |> field("number", int)}
      )
    | _ => StringEvent("Unknown")
    };
  let encode = (payload) =>
    switch payload {
    | StringEvent(text) => text
    | IntEvent(n) => Json.Encode.int(n) |> Json.stringify
    | DataEvent(d) =>
      Json.Encode.(object_([("text", string(d.text)), ("number", int(d.number))]))
      |> Json.stringify
    };
  let getType = (payload) =>
    switch payload {
    | StringEvent(_) => "StringEvent"
    | IntEvent(_) => "IntEvent"
    | DataEvent(_) => "DataEvent"
    };
};

let () = {
  module TestLog = Log.Make(TestPayload);
  let keyPair = Bitcoin.ECPair.makeRandom();
  describe(
    "Encode/Decode",
    () => {
      test(
        "StringEvent",
        () => {
          let event = TestPayload.StringEvent("hello") |> TestLog.createEvent(keyPair);
          let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
          expect(event) |> toEqual(decoded)
        }
      );
      test(
        "IntEvent",
        () => {
          let event = TestPayload.IntEvent(1) |> TestLog.createEvent(keyPair);
          let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
          expect(event) |> toEqual(decoded)
        }
      );
      test(
        "DataEvent",
        () => {
          let event =
            TestPayload.DataEvent({text: "text", number: 1}) |> TestLog.createEvent(keyPair);
          let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
          expect(event) |> toEqual(decoded)
        }
      )
    }
  );
  test(
    "sign/verify",
    () => {
      open Bitcoin;
      let event = TestPayload.StringEvent("hello") |> TestLog.createEvent(keyPair);
      expect(event.signature)
      |> toBe(
           keyPair
           |> ECPair.sign(
                BufferExt.fromStringWithEncoding(
                  "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
                  "hex"
                )
              )
           |> ECSignature.toDER
           |> BufferExt.toStringWithEncoding("hex")
         )
    }
  )
};
