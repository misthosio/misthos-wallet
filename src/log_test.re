open Jest;

open Expect;

module TestItem = {
  type t = string;
  let decode = (item, ~type_) => item;
  let encode = (item) => item;
  let getType = (_item) => "string";
  type validatorState = int;
  let canKeySignItem = (_state, _item, _pubKey) => true;
  let canKeyWitnessItem = (_state, _item, _pubKey) => true;
  let validate = (state, _item) => (true, state);
};

let () = {
  module TestLog = Log.Make(TestItem);
  let keyPair = Bitcoin.ECPair.makeRandom();
  describe(
    "append",
    () =>
      test(
        "valid append",
        () => {
          let log = TestLog.make(0);
          let log = log |> TestLog.append("hello", keyPair);
          let log = log |> TestLog.append(" - bye", keyPair);
          expect(log |> TestLog.reduce((state, entry) => state ++ entry, ""))
          |> toEqual("hello - bye")
        }
      )
  )
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
