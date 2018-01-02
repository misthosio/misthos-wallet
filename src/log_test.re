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
          expect(event.payload) |> toEqual(decoded.payload)
        }
      );
      test(
        "IntEvent",
        () => {
          let event = TestPayload.IntEvent(1) |> TestLog.createEvent(keyPair);
          let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
          expect(event.payload) |> toEqual(decoded.payload)
        }
      );
      test(
        "DataEvent",
        () => {
          let event =
            TestPayload.DataEvent({text: "text", number: 1}) |> TestLog.createEvent(keyPair);
          let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
          expect(event.payload) |> toEqual(decoded.payload)
        }
      )
    }
  );
  test(
    "sign/verify",
    () => {
      open Bitcoin;
      let event = TestPayload.StringEvent("hello") |> TestLog.createEvent(keyPair);
      let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event;
      let verified =
        keyPair
        |> ECPair.verify(
             decoded.payloadHash |> BufferExt.fromStringWithEncoding(~encoding="hex"),
             decoded.signature
           );
      expect(verified) |> toEqual(Js.true_)
    }
  )
};
