open WorkerLocalStorage;

open PrimitiveTypes;

open WalletTypes;

type incoming =
  | UpdateSession(blockstackItems)
  | Create(string)
  | Load(ventureId)
  | JoinVia(ventureId, userId)
  | ProposePartner(ventureId, userId)
  | RejectPartner(ventureId, processId)
  | EndorsePartner(ventureId, processId)
  | ProposePartnerRemoval(ventureId, userId)
  | RejectPartnerRemoval(ventureId, processId)
  | EndorsePartnerRemoval(ventureId, processId)
  | ProposePayout(ventureId, accountIdx, list((string, BTC.t)), BTC.t)
  | RejectPayout(ventureId, processId)
  | EndorsePayout(ventureId, processId)
  | ExposeIncomeAddress(ventureId, accountIdx)
  | NewItemsDetected(ventureId, array(EventLog.item))
  | SyncWallet(
      ventureId,
      list(Event.IncomeDetected.t),
      list(Event.Transaction.Confirmed.t),
    )
  | SyncTabs(ventureId, array(EventLog.item));

type encodedIncoming = Js.Json.t;

type outgoing =
  | UpdateIndex(Venture.Index.t)
  | VentureLoaded(ventureId, array(EventLog.item), array(EventLog.item))
  | VentureCreated(ventureId, array(EventLog.item))
  | NewItems(ventureId, array(EventLog.item));

type encodedOutgoing = Js.Json.t;

exception UnknownMessage(Js.Json.t);

let encodeIncoming =
  fun
  | UpdateSession(blockstackItems) =>
    Json.Encode.(
      object_([
        ("type", string("UpdateSession")),
        ("blockstackItems", WorkerLocalStorage.encodeItems(blockstackItems)),
      ])
    )
  | Create(name) =>
    Json.Encode.(
      object_([("type", string("Create")), ("name", string(name))])
    )
  | Load(ventureId) =>
    Json.Encode.(
      object_([
        ("type", string("Load")),
        ("ventureId", VentureId.encode(ventureId)),
      ])
    )
  | JoinVia(ventureId, userId) =>
    Json.Encode.(
      object_([
        ("type", string("JoinVia")),
        ("ventureId", VentureId.encode(ventureId)),
        ("userId", UserId.encode(userId)),
      ])
    )
  | ProposePartner(ventureId, userId) =>
    Json.Encode.(
      object_([
        ("type", string("ProposePartner")),
        ("ventureId", VentureId.encode(ventureId)),
        ("userId", UserId.encode(userId)),
      ])
    )
  | RejectPartner(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("RejectPartner")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | EndorsePartner(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("EndorsePartner")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | ProposePartnerRemoval(ventureId, userId) =>
    Json.Encode.(
      object_([
        ("type", string("ProposePartnerRemoval")),
        ("ventureId", VentureId.encode(ventureId)),
        ("userId", UserId.encode(userId)),
      ])
    )
  | RejectPartnerRemoval(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("RejectPartnerRemoval")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | EndorsePartnerRemoval(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("EndorsePartnerRemoval")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | ProposePayout(ventureId, accountIdx, destinations, fee) =>
    Json.Encode.(
      object_([
        ("type", string("ProposePayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("accountIdx", AccountIndex.encode(accountIdx)),
        ("destinations", list(tuple2(string, BTC.encode), destinations)),
        ("fee", BTC.encode(fee)),
      ])
    )
  | RejectPayout(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("RejectPayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | EndorsePayout(ventureId, processId) =>
    Json.Encode.(
      object_([
        ("type", string("EndorsePayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | ExposeIncomeAddress(ventureId, accountIdx) =>
    Json.Encode.(
      object_([
        ("type", string("ExposeIncomeAddress")),
        ("ventureId", VentureId.encode(ventureId)),
        ("accountIdx", AccountIndex.encode(accountIdx)),
      ])
    )
  | NewItemsDetected(ventureId, items) =>
    Json.Encode.(
      object_([
        ("type", string("NewItemsDetected")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
      ])
    )
  | SyncWallet(ventureId, incomeEvents, confs) =>
    Json.Encode.(
      object_([
        ("type", string("SyncWallet")),
        ("ventureId", VentureId.encode(ventureId)),
        ("incomeEvents", list(Event.IncomeDetected.encode, incomeEvents)),
        (
          "transactionConfirmations",
          list(Event.Transaction.Confirmed.encode, confs),
        ),
      ])
    )
  | SyncTabs(ventureId, items) =>
    Json.Encode.(
      object_([
        ("type", string("SyncTabs")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
      ])
    );

let decodeIncoming = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "UpdateSession" =>
    let blockstackItems =
      raw
      |> Json.Decode.field("blockstackItems", WorkerLocalStorage.decodeItems);
    UpdateSession(blockstackItems);
  | "Create" =>
    let name = raw |> Json.Decode.(field("name", string));
    Create(name);
  | "Load" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    Load(ventureId);
  | "JoinVia" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let userId = raw |> Json.Decode.field("userId", UserId.decode);
    JoinVia(ventureId, userId);
  | "ProposePartner" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let userId = raw |> Json.Decode.field("userId", UserId.decode);
    ProposePartner(ventureId, userId);
  | "RejectPartner" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    RejectPartner(ventureId, processId);
  | "EndorsePartner" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    EndorsePartner(ventureId, processId);
  | "ProposePartnerRemoval" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let userId = raw |> Json.Decode.field("userId", UserId.decode);
    ProposePartnerRemoval(ventureId, userId);
  | "RejectPartnerRemoval" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    RejectPartnerRemoval(ventureId, processId);
  | "EndorsePartnerRemoval" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    EndorsePartnerRemoval(ventureId, processId);
  | "ProposePayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let accountIdx =
      raw |> Json.Decode.field("accountIdx", AccountIndex.decode);
    let destinations =
      raw
      |> Json.Decode.(
           field("destinations", list(tuple2(string, BTC.decode)))
         );
    let fee = raw |> Json.Decode.field("fee", BTC.decode);
    ProposePayout(ventureId, accountIdx, destinations, fee);
  | "RejectPayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    RejectPayout(ventureId, processId);
  | "EndorsePayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    EndorsePayout(ventureId, processId);
  | "ExposeIncomeAddress" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let accountIdx =
      raw |> Json.Decode.field("accountIdx", AccountIndex.decode);
    ExposeIncomeAddress(ventureId, accountIdx);
  | "SyncWallet" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let incomeEvents =
      raw
      |> Json.Decode.(
           field("incomeEvents", list(Event.IncomeDetected.decode))
         );
    let confs =
      raw
      |> Json.Decode.(
           field(
             "transactionConfirmations",
             list(Event.Transaction.Confirmed.decode),
           )
         );
    SyncWallet(ventureId, incomeEvents, confs);
  | "NewItemsDetected" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      raw |> Json.Decode.(field("items", array(EventLog.decodeItem)));
    NewItemsDetected(ventureId, items);
  | "SyncTabs" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      raw |> Json.Decode.(field("items", array(EventLog.decodeItem)));
    SyncTabs(ventureId, items);
  | _ => raise(UnknownMessage(raw))
  };
};

let encodeOutgoing =
  fun
  | UpdateIndex(index) =>
    Json.Encode.(
      object_([
        ("type", string("UpdateIndex")),
        ("index", Venture.Index.encode(index)),
      ])
    )
  | VentureCreated(ventureId, items) =>
    Json.Encode.(
      object_([
        ("type", string("VentureCreated")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
      ])
    )
  | VentureLoaded(ventureId, items, newItems) =>
    Json.Encode.(
      object_([
        ("type", string("VentureLoaded")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
        ("newItems", array(EventLog.encodeItem, newItems)),
      ])
    )
  | NewItems(ventureId, items) =>
    Json.Encode.(
      object_([
        ("type", string("NewItems")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
      ])
    );

let decodeOutgoing = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "VentureCreated" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      Json.Decode.(raw |> field("items", array(EventLog.decodeItem)));
    VentureCreated(ventureId, items);
  | "VentureLoaded" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      Json.Decode.(raw |> field("items", array(EventLog.decodeItem)));
    let newItems =
      Json.Decode.(raw |> field("newItems", array(EventLog.decodeItem)));
    VentureLoaded(ventureId, items, newItems);
  | "NewItems" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      Json.Decode.(raw |> field("items", array(EventLog.decodeItem)));
    NewItems(ventureId, items);
  | "UpdateIndex" =>
    UpdateIndex(raw |> Json.Decode.field("index", Venture.Index.decode))
  | _ => raise(UnknownMessage(raw))
  };
};
