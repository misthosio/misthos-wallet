open Jest;

open Expect;

module TestItem = {
  type t = string;
  let decode = Json.Decode.string;
  let encode = Json.Encode.string;
};

let () = {
  module TestLog = Log.Make(TestItem);
  let print = log => Js.log(log |> TestLog.encode);
  let reduceLog = log =>
    log |> TestLog.reduce((state, {event}) => state ++ event, "");
  test("append/reduce", () => {
    let keyPair = Bitcoin.ECPair.makeRandom();
    let log =
      TestLog.make()
      |> TestLog.append(keyPair, "hello")
      |> snd
      |> TestLog.append(keyPair, " - bye")
      |> snd;
    expect(log |> reduceLog) |> toEqual("hello - bye");
  });
  /* describe("merge", () => { */
  /*   let issuerA = Bitcoin.ECPair.makeRandom(); */
  /*   let issuerAPubKey = issuerA |> Utils.publicKeyFromKeyPair; */
  /*   let issuerB = Bitcoin.ECPair.makeRandom(); */
  /*   let issuerBPubKey = issuerB |> Utils.publicKeyFromKeyPair; */
  /*   test("merge 0", () => { */
  /*     let logA = TestLog.make() |> TestLog.append("A1", issuerA); */
  /*     let log = logA |> TestLog.merge(issuerA, [], []); */
  /*     expect(reduceLog(log)) |> toEqual("A1"); */
  /*   }); */
  /*   describe("merge 1", () => { */
  /*     let logA = TestLog.make() |> TestLog.append("A1", issuerA); */
  /*     let logB = logA |> TestLog.append("B1", issuerB); */
  /*     test("result merge A", () => { */
  /*       let result = logA |> TestLog.merge(issuerA, [issuerBPubKey], [logB]); */
  /*       expect(result |> reduceLog) |> toEqual("A1B1"); */
  /*     }); */
  /*     test("result merge B", () => { */
  /*       let result = logB |> TestLog.merge(issuerB, [issuerAPubKey], [logA]); */
  /*       expect(result |> reduceLog) |> toEqual("A1B1"); */
  /*     }); */
  /*   }); */
  /* }); */
  /* describe("merge 1 w/ reorg", () => { */
  /*   let shared = */
  /*     TestLog.make() */
  /*     |> TestLog.append("A1", issuerA) */
  /*     |> TestLog.append("B1", issuerB); */
  /*   let logA = shared |> TestLog.append("A2", issuerA); */
  /*   let logB = shared |> TestLog.append("B2", issuerB); */
  /*   test("result merge A", () => { */
  /*     let result = logA |> TestLog.merge(issuerA, [issuerBPubKey], [logB]); */
  /*     expect(result |> reduceLog) |> toEqual("A1B1A2"); */
  /*   }); */
  /* }); */
  /* test("result merge B", () => { */
  /*   let result = logB |> TestLog.merge(issuerB, [issuerAPubKey], [logA]); */
  /*   expect(result |> reduceLog) |> toEqual("A1B1A2B2"); */
  /* }); */
  /* expect(TestLog.head(resultA)) |> toEqual(TestLog.head(resultA)) */
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
