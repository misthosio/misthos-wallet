open Jest;

open Expect;

module TestItem = {
  type t = string;
  let decode = item => item;
  let encode = item => item;
};

let () = {
  module TestLog = WitnessedLog.Make(TestItem);
  let keyPair = Bitcoin.ECPair.makeRandom();
  test("append/reduce", () => {
    let log = TestLog.make();
    let log =
      log
      |> TestLog.append("hello", keyPair)
      |> TestLog.append(" - bye", keyPair);
    expect(log |> TestLog.reduce((state, entry) => state ++ entry, ""))
    |> toEqual("hello - bye");
  });
  /* describe( */
  /*   "Encode/Decode", */
  /*   () => { */
  /*     test( */
  /*       "StringEvent", */
  /*       () => { */
  /*         let event = TestPayload.StringEvent("hello") |> TestLog.createEvent(keyPair); */
  /*         let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event; */
  /*         expect(event.payload) |> toEqual(decoded.payload) */
  /*       } */
  /*     ); */
  /*     test( */
  /*       "IntEvent", */
  /*       () => { */
  /*         let event = TestPayload.IntEvent(1) |> TestLog.createEvent(keyPair); */
  /*         let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event; */
  /*         expect(event.payload) |> toEqual(decoded.payload) */
  /*       } */
  /*     ); */
  /*     test( */
  /*       "DataEvent", */
  /*       () => { */
  /*         let event = */
  /*           TestPayload.DataEvent({text: "text", number: 1}) |> TestLog.createEvent(keyPair); */
  /*         let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event; */
  /*         expect(event.payload) |> toEqual(decoded.payload) */
  /*       } */
  /*     ) */
  /*   } */
  /* ); */
  /* test( */
  /*   "sign/verify", */
  /*   () => { */
  /*     open Bitcoin; */
  /*     let event = TestPayload.StringEvent("hello") |> TestLog.createEvent(keyPair); */
  /*     let decoded = event |> TestLog.Encode.event |> TestLog.Decode.event; */
  /*     let verified = */
  /*       keyPair */
  /*       |> ECPair.verify( */
  /*            decoded.payloadHash |> BufferExt.fromStringWithEncoding(~encoding="hex"), */
  /*            decoded.signature */
  /*          ); */
  /*     expect(verified) |> toEqual(Js.true_) */
  /*   } */
  /* ) */
};
