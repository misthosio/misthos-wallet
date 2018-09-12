open WorkerLocalStorage;

open PrimitiveTypes;

open WalletTypes;

type incoming =
  | UpdateSession(blockstackItems)
  | Create(string, AccountSettings.t, Policy.initialPolicies)
  | Load(ventureId)
  | JoinVia(ventureId, userId)
  | RegisterIntegration(ventureId, string)
  | ProposePartner(ventureId, userId)
  | RejectPartner(ventureId, processId)
  | EndorsePartner(ventureId, processId)
  | ProposePartnerRemoval(ventureId, userId)
  | RejectPartnerRemoval(ventureId, processId)
  | EndorsePartnerRemoval(ventureId, processId)
  | SubmitCustodianKeyChain(ventureId, CustodianKeyChain.public)
  | ProposePayout(
      ventureId,
      accountIdx,
      PayoutTransaction.t,
      array(option((string, string))),
    )
  | RejectPayout(ventureId, processId)
  | EndorsePayout(ventureId, array(option((string, string))), processId)
  | SignPayout(ventureId, array(option((string, string))), processId)
  | ExposeIncomeAddress(ventureId, accountIdx)
  | NewItemsDetected(ventureId, array(EventLog.item), userId)
  | SyncWallet(
      ventureId,
      list(Event.Payout.Broadcast.t),
      list(Event.Payout.BroadcastFailed.t),
      list(Event.Income.Detected.t),
      list(Event.Income.Unlocked.t),
      list(Event.Transaction.Confirmed.t),
    )
  | SyncTabs(ventureId, array(EventLog.item));

type encodedIncoming = Js.Json.t;

type cmdSuccess =
  | KeyChainSubmitted
  | TransactionSigned
  | IntegrationRegistered
  | ProcessStarted(processId)
  | ProcessEndorsed(processId)
  | ProcessRejected(processId);

type cmdError =
  | NotACustodian
  | CouldNotJoinVenture
  | CouldNotLoadVenture
  | MaxPartnersReached
  | PartnerAlreadyExists
  | PartnerAlreadyProposed
  | UserIdDoesNotExist
  | CouldNotPersistVenture;

type cmdResponse =
  | Ok(cmdSuccess)
  | Error(cmdError);

type outgoing =
  | SessionStarted(blockstackItems, string)
  | SessionPending
  | NewIncomeAddress(ventureId, string)
  | UpdateIndex(Venture.Index.t)
  | VentureLoaded(ventureId, EventLog.t, array(EventLog.item))
  | VentureCreated(ventureId, EventLog.t)
  | NewItems(ventureId, array(EventLog.item))
  | CmdCompleted(ventureId, WebWorker.correlationId, cmdResponse);

type encodedOutgoing = Js.Json.t;

exception UnknownMessage(Js.Json.t);

let encodeSuccess =
  fun
  | KeyChainSubmitted =>
    Json.Encode.(object_([("type", string("KeyChainSubmitted"))]))
  | TransactionSigned =>
    Json.Encode.(object_([("type", string("TransactionSigned"))]))
  | IntegrationRegistered =>
    Json.Encode.(object_([("type", string("IntegrationRegistered"))]))
  | ProcessStarted(processId) =>
    Json.Encode.(
      object_([
        ("type", string("ProcessStarted")),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | ProcessRejected(processId) =>
    Json.Encode.(
      object_([
        ("type", string("ProcessRejected")),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | ProcessEndorsed(processId) =>
    Json.Encode.(
      object_([
        ("type", string("ProcessEndorsed")),
        ("processId", ProcessId.encode(processId)),
      ])
    );

let decodeSuccess = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "KeyChainSubmitted" => KeyChainSubmitted
  | "TransactionSigned" => TransactionSigned
  | "IntegrationRegistered" => IntegrationRegistered
  | "ProcessStarted" =>
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    ProcessStarted(processId);
  | "ProcessEndorsed" =>
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    ProcessEndorsed(processId);
  | "ProcessRejected" =>
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    ProcessRejected(processId);
  | _ => raise(UnknownMessage(raw))
  };
};

let encodeError =
  fun
  | NotACustodian =>
    Json.Encode.(object_([("type", string("NotACustodian"))]))
  | CouldNotJoinVenture =>
    Json.Encode.(object_([("type", string("CouldNotJoinVenture"))]))
  | CouldNotLoadVenture =>
    Json.Encode.(object_([("type", string("CouldNotLoadVenture"))]))
  | MaxPartnersReached =>
    Json.Encode.(object_([("type", string("MaxPartnersReached"))]))
  | PartnerAlreadyExists =>
    Json.Encode.(object_([("type", string("PartnerAlreadyExists"))]))
  | PartnerAlreadyProposed =>
    Json.Encode.(object_([("type", string("PartnerAlreadyProposed"))]))
  | UserIdDoesNotExist =>
    Json.Encode.(object_([("type", string("UserIdDoesNotExist"))]))
  | CouldNotPersistVenture =>
    Json.Encode.(object_([("type", string("CouldNotPersistVenture"))]));

let decodeError = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "CouldNotJoinVenture" => CouldNotJoinVenture
  | "CouldNotLoadVenture" => CouldNotLoadVenture
  | "MaxPartnersReached" => MaxPartnersReached
  | "PartnerAlreadyProposed" => PartnerAlreadyProposed
  | "PartnerAlreadyExists" => PartnerAlreadyExists
  | "UserIdDoesNotExist" => UserIdDoesNotExist
  | "CouldNotPersistVenture" => CouldNotPersistVenture
  | _ => raise(UnknownMessage(raw))
  };
};

let encodeResponse =
  fun
  | Ok(cmdSuccess) =>
    Json.Encode.(
      object_([
        ("type", string("Ok")),
        ("cmdSuccess", encodeSuccess(cmdSuccess)),
      ])
    )
  | Error(cmdError) =>
    Json.Encode.(
      object_([
        ("type", string("Error")),
        ("cmdError", encodeError(cmdError)),
      ])
    );

let decodeResponse = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "Ok" => Ok(raw |> Json.Decode.field("cmdSuccess", decodeSuccess))
  | "Error" => Error(raw |> Json.Decode.field("cmdError", decodeError))
  | _ => raise(UnknownMessage(raw))
  };
};

let encodeIncoming =
  fun
  | UpdateSession(blockstackItems) =>
    Json.Encode.(
      object_([
        ("type", string("UpdateSession")),
        ("blockstackItems", WorkerLocalStorage.encodeItems(blockstackItems)),
      ])
    )
  | Create(name, accountSettings, initialPolicies) =>
    Json.Encode.(
      object_([
        ("type", string("Create")),
        ("name", string(name)),
        ("accountSettings", AccountSettings.encode(accountSettings)),
        ("initialPolicies", Policy.encodeInitialPolicies(initialPolicies)),
      ])
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
  | RegisterIntegration(ventureId, integrationPubKey) =>
    Json.Encode.(
      object_([
        ("type", string("RegisterIntegration")),
        ("ventureId", VentureId.encode(ventureId)),
        ("integrationPubKey", string(integrationPubKey)),
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
  | SubmitCustodianKeyChain(ventureId, keyChain) =>
    Json.Encode.(
      object_([
        ("type", string("SubmitCustodianKeyChain")),
        ("ventureId", VentureId.encode(ventureId)),
        ("keyChain", CustodianKeyChain.encode(keyChain)),
      ])
    )
  | ProposePayout(ventureId, accountIdx, payoutTx, signatures) =>
    Json.Encode.(
      object_([
        ("type", string("ProposePayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("accountIdx", AccountIndex.encode(accountIdx)),
        ("payoutTx", PayoutTransaction.encode(payoutTx)),
        ("signatures", array(nullable(pair(string, string)), signatures)),
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
  | EndorsePayout(ventureId, signatures, processId) =>
    Json.Encode.(
      object_([
        ("type", string("EndorsePayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("signatures", array(nullable(pair(string, string)), signatures)),
        ("processId", ProcessId.encode(processId)),
      ])
    )
  | SignPayout(ventureId, signatures, processId) =>
    Json.Encode.(
      object_([
        ("type", string("SignPayout")),
        ("ventureId", VentureId.encode(ventureId)),
        ("signatures", array(nullable(pair(string, string)), signatures)),
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
  | NewItemsDetected(ventureId, items, partnerId) =>
    Json.Encode.(
      object_([
        ("type", string("NewItemsDetected")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
        ("partnerId", UserId.encode(partnerId)),
      ])
    )
  | SyncWallet(
      ventureId,
      broadcasts,
      broadcastFailures,
      incomeEvents,
      unlockEvents,
      confs,
    ) =>
    Json.Encode.(
      object_([
        ("type", string("SyncWallet")),
        ("ventureId", VentureId.encode(ventureId)),
        ("broadcastEvents", list(Event.Payout.Broadcast.encode, broadcasts)),
        (
          "broadcastFailures",
          list(Event.Payout.BroadcastFailed.encode, broadcastFailures),
        ),
        ("incomeEvents", list(Event.Income.Detected.encode, incomeEvents)),
        ("unlockEvents", list(Event.Income.Unlocked.encode, unlockEvents)),
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
    let accountSettings =
      raw |> Json.Decode.(field("accountSettings", AccountSettings.decode));
    let initialPolicies =
      raw
      |> Json.Decode.(field("initialPolicies", Policy.decodeInitialPolicies));
    Create(name, accountSettings, initialPolicies);
  | "Load" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    Load(ventureId);
  | "JoinVia" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let userId = raw |> Json.Decode.field("userId", UserId.decode);
    JoinVia(ventureId, userId);
  | "RegisterIntegration" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let integrationPubKey =
      raw |> Json.Decode.(field("integrationPubKey", string));
    RegisterIntegration(ventureId, integrationPubKey);
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
  | "SubmitCustodianKeyChain" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let keyChain =
      raw |> Json.Decode.field("keyChain", CustodianKeyChain.decode);
    SubmitCustodianKeyChain(ventureId, keyChain);

  | "ProposePayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let accountIdx =
      raw |> Json.Decode.field("accountIdx", AccountIndex.decode);
    let payoutTx =
      raw |> Json.Decode.(field("payoutTx", PayoutTransaction.decode));
    let signatures =
      raw
      |> Json.Decode.(
           field("signatures", array(optional(pair(string, string))))
         );
    ProposePayout(ventureId, accountIdx, payoutTx, signatures);
  | "RejectPayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    RejectPayout(ventureId, processId);
  | "EndorsePayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    let signatures =
      raw
      |> Json.Decode.(
           field("signatures", array(optional(pair(string, string))))
         );
    EndorsePayout(ventureId, signatures, processId);
  | "SignPayout" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let processId = raw |> Json.Decode.field("processId", ProcessId.decode);
    let signatures =
      raw
      |> Json.Decode.(
           field("signatures", array(optional(pair(string, string))))
         );
    SignPayout(ventureId, signatures, processId);
  | "ExposeIncomeAddress" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let accountIdx =
      raw |> Json.Decode.field("accountIdx", AccountIndex.decode);
    ExposeIncomeAddress(ventureId, accountIdx);
  | "SyncWallet" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let broadcasts =
      raw
      |> Json.Decode.(
           field("broadcastEvents", list(Event.Payout.Broadcast.decode))
         );
    let broadcastFailures =
      raw
      |> Json.Decode.(
           field(
             "broadcastFailures",
             list(Event.Payout.BroadcastFailed.decode),
           )
         );
    let incomeEvents =
      raw
      |> Json.Decode.(
           field("incomeEvents", list(Event.Income.Detected.decode))
         );
    let unlockEvents =
      raw
      |> Json.Decode.(
           field("unlockEvents", list(Event.Income.Unlocked.decode))
         );
    let confs =
      raw
      |> Json.Decode.(
           field(
             "transactionConfirmations",
             list(Event.Transaction.Confirmed.decode),
           )
         );
    SyncWallet(
      ventureId,
      broadcasts,
      broadcastFailures,
      incomeEvents,
      unlockEvents,
      confs,
    );
  | "NewItemsDetected" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      raw |> Json.Decode.(field("items", array(EventLog.decodeItem)));
    let partnerId = raw |> Json.Decode.(field("partnerId", UserId.decode));
    NewItemsDetected(ventureId, items, partnerId);
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
  | SessionStarted(blockstackItems, storagePrefix) =>
    Json.Encode.(
      object_([
        ("type", string("SessionStarted")),
        ("blockstackItems", WorkerLocalStorage.encodeItems(blockstackItems)),
        ("storagePrefix", string(storagePrefix)),
      ])
    )
  | SessionPending =>
    Json.Encode.(object_([("type", string("SessionPending"))]))
  | UpdateIndex(index) =>
    Json.Encode.(
      object_([
        ("type", string("UpdateIndex")),
        ("index", Venture.Index.encode(index)),
      ])
    )
  | VentureCreated(ventureId, log) =>
    Json.Encode.(
      object_([
        ("type", string("VentureCreated")),
        ("ventureId", VentureId.encode(ventureId)),
        ("log", EventLog.encode(log)),
      ])
    )
  | VentureLoaded(ventureId, log, newItems) =>
    Json.Encode.(
      object_([
        ("type", string("VentureLoaded")),
        ("ventureId", VentureId.encode(ventureId)),
        ("log", EventLog.encode(log)),
        ("newItems", array(EventLog.encodeItem, newItems)),
      ])
    )
  | NewIncomeAddress(ventureId, address) =>
    Json.Encode.(
      object_([
        ("type", string("NewIncomeAddress")),
        ("ventureId", VentureId.encode(ventureId)),
        ("address", string(address)),
      ])
    )
  | NewItems(ventureId, items) =>
    Json.Encode.(
      object_([
        ("type", string("NewItems")),
        ("ventureId", VentureId.encode(ventureId)),
        ("items", array(EventLog.encodeItem, items)),
      ])
    )
  | CmdCompleted(ventureId, correlationId, response) =>
    Json.Encode.(
      object_([
        ("type", string("CmdCompleted")),
        ("ventureId", VentureId.encode(ventureId)),
        ("correlationId", string(correlationId)),
        ("response", encodeResponse(response)),
      ])
    );

let decodeOutgoing = raw => {
  let type_ = raw |> Json.Decode.(field("type", string));
  switch (type_) {
  | "SessionStarted" =>
    let blockstackItems =
      raw
      |> Json.Decode.field("blockstackItems", WorkerLocalStorage.decodeItems);
    let storagePrefix = raw |> Json.Decode.(field("storagePrefix", string));
    SessionStarted(blockstackItems, storagePrefix);
  | "SessionPending" => SessionPending
  | "VentureCreated" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let log = Json.Decode.(raw |> field("log", EventLog.decode));
    VentureCreated(ventureId, log);
  | "VentureLoaded" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let log = Json.Decode.(raw |> field("log", EventLog.decode));
    let newItems =
      Json.Decode.(raw |> field("newItems", array(EventLog.decodeItem)));
    VentureLoaded(ventureId, log, newItems);
  | "NewIncomeAddress" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let address = raw |> Json.Decode.(field("address", string));
    NewIncomeAddress(ventureId, address);
  | "NewItems" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let items =
      Json.Decode.(raw |> field("items", array(EventLog.decodeItem)));
    NewItems(ventureId, items);
  | "CmdCompleted" =>
    let ventureId = raw |> Json.Decode.field("ventureId", VentureId.decode);
    let correlationId = raw |> Json.Decode.(field("correlationId", string));
    let response = raw |> Json.Decode.field("response", decodeResponse);
    CmdCompleted(ventureId, correlationId, response);
  | "UpdateIndex" =>
    UpdateIndex(raw |> Json.Decode.field("index", Venture.Index.decode))
  | _ => raise(UnknownMessage(raw))
  };
};
