open WorkerLocalStorage;

open PrimitiveTypes;

open WalletTypes;

type incoming =
  | UpdateSession(blockstackItems)
  | Create(string)
  | Load(ventureId)
  | JoinVia(ventureId, userId)
  | ProposePartner(ventureId, userId)
  | EndorsePartner(ventureId, processId)
  | ProposePartnerRemoval(ventureId, userId)
  | EndorsePartnerRemoval(ventureId, processId)
  | ProposePayout(
      ventureId,
      accountIdx,
      list((string, BTC.encoded)),
      BTC.encoded,
    )
  | EndorsePayout(ventureId, processId)
  | ExposeIncomeAddress(ventureId, accountIdx)
  | TransactionDetected(ventureId, list(Js.Json.t));

type outgoing =
  | UpdateIndex(Venture.Index.t)
  | VentureLoaded(ventureId, list(Event.t))
  | VentureCreated(ventureId, list(Event.t))
  | NewEvents(ventureId, list(Event.t));

type encodedOutgoing = Js.Json.t;

let encodeOutgoing =
  fun
  | UpdateIndex(index) =>
    Json.Encode.(
      object_([
        ("type", string("UpdateIndex")),
        ("index", Venture.Index.encode(index)),
      ])
    )
  | VentureCreated(ventureId, events) =>
    Json.Encode.(
      object_([
        ("type", string("VentureCreated")),
        ("ventureId", VentureId.encode(ventureId)),
        ("events", list(Event.encode, events)),
      ])
    )
  | VentureLoaded(ventureId, events) =>
    Json.Encode.(
      object_([
        ("type", string("VentureLoaded")),
        ("ventureId", VentureId.encode(ventureId)),
        ("events", list(Event.encode, events)),
      ])
    )
  | NewEvents(ventureId, events) =>
    Json.Encode.(
      object_([
        ("type", string("NewEvents")),
        ("ventureId", VentureId.encode(ventureId)),
        ("events", list(Event.encode, events)),
      ])
    );

exception UnknownMessage(Js.Json.t);

let decodeOutgoing = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "VentureCreated" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let events = Json.Decode.(raw |> field("events", list(Event.decode)));
    VentureCreated(ventureId, events);
  | "VentureLoaded" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let events = Json.Decode.(raw |> field("events", list(Event.decode)));
    VentureLoaded(ventureId, events);
  | "NewEvents" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let events = Json.Decode.(raw |> field("events", list(Event.decode)));
    NewEvents(ventureId, events);
  | "UpdateIndex" =>
    UpdateIndex(raw |> Json.Decode.field("index", Venture.Index.decode))
  | _ => raise(UnknownMessage(raw))
  };
};
