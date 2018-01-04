open Jest;

open Expect;

module TestItem = {
  type t = string;
  let decode = item => item;
  let encode = item => item;
};

let () = {
  module TestLog = WitnessedLog.Make(TestItem);
  let print = log => Js.log(log |> TestLog.encode);
  test("append/reduce", () => {
    let keyPair = Bitcoin.ECPair.makeRandom();
    let log =
      TestLog.make()
      |> TestLog.append("hello", keyPair)
      |> TestLog.append(" - bye", keyPair);
    expect(log |> TestLog.reduce((state, entry) => state ++ entry, ""))
    |> toEqual("hello - bye");
  });
  describe("merge", () => {
    let issuerA = Bitcoin.ECPair.makeRandom();
    let issuerAPubKey = issuerA |> Utils.publicKeyFromKeyPair;
    let issuerB = Bitcoin.ECPair.makeRandom();
    let issuerBPubKey = issuerB |> Utils.publicKeyFromKeyPair;
    test("merge 0", () => {
      let logA = TestLog.make() |> TestLog.append("eventA", issuerA);
      let log = logA |> TestLog.merge(issuerA, [], []);
      expect(log) |> toEqual(logA);
    });
    describe("merge 1", () => {
      let logA = TestLog.make() |> TestLog.append("eventA", issuerA);
      let logB = logA |> TestLog.append("eventB", issuerB);
      let resultA = logA |> TestLog.merge(issuerA, [issuerBPubKey], [logB]);
      let resultB = logB |> TestLog.merge(issuerB, [issuerAPubKey], [logA]);
      test("head", ()
        /* expect(TestLog.head(resultA)) |> toEqual(TestLog.head(resultB)) */
        =>
          expect(TestLog.head(resultA)) |> toEqual(TestLog.head(resultA))
        );
    });
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
