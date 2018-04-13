open PrimitiveTypes;

open WalletTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

exception InvalidEvent(Validation.result);

exception CouldNotLoadVenture;

type t = {
  session: Session.Data.t,
  id: ventureId,
  log: EventLog.t,
  state: Validation.state,
  wallet: Venture__Wallet.t,
  viewModel: ViewModel.t,
  watchers: Watchers.t,
};

module Wallet = {
  include Venture__Wallet;
  let balance = ({wallet}) => balance(AccountIndex.default, wallet);
};

let make = (session, id) => {
  session,
  id,
  log: EventLog.make(),
  state: Validation.makeState(),
  wallet: Wallet.make(),
  viewModel: ViewModel.make(),
  watchers: [],
};

let applyInternal = (issuer, event, log, (state, wallet, viewModel)) => {
  logMessage("Appending event to log:");
  logMessage(Event.encode(event) |> Json.stringify);
  let (item, log) = log |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(state)) {
  | Ok =>
    let state = state |> Validation.apply(event);
    let wallet = wallet |> Wallet.apply(event);
    let viewModel = viewModel |> ViewModel.apply(event);
    (item, log, (state, wallet, viewModel));
  /* This should never happen / only incase of an UI input bug!!! */
  | result =>
    logMessage("Event was rejected because of:");
    logMessage(Validation.resultToString(result));
    raise(InvalidEvent(result));
  };
};

let apply = (event, {session, id, log, state, wallet, viewModel, watchers}) => {
  let (item, log, (state, wallet, viewModel)) =
    applyInternal(
      session.issuerKeyPair,
      event,
      log,
      (state, wallet, viewModel),
    );
  Js.Promise.(
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal,
         (state, wallet, viewModel),
       )
    |> then_(((log, (state, wallet, viewModel), watchers)) =>
         {session, id, log, state, wallet, viewModel, watchers} |> resolve
       )
  );
};

let reconstruct = (session, log) => {
  let {viewModel, state, wallet} = make(session, VentureId.make());
  let (id, state, wallet, viewModel, watchers) =
    log
    |> EventLog.reduce(
         ((id, state, wallet, viewModel, watchers), {event} as item) => (
           switch (event) {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           state |> Validation.apply(event),
           wallet |> Wallet.apply(event),
           viewModel |> ViewModel.apply(event),
           watchers |> Watchers.apply(~reconstruct=true, session, item, log),
         ),
         (VentureId.make(), state, wallet, viewModel, []),
       );
  watchers
  |> Watchers.processPending(
       session,
       log,
       applyInternal,
       (state, wallet, viewModel),
     )
  |> Js.Promise.then_(((log, (state, wallet, viewModel), watchers)) =>
       {session, id, log, state, wallet, viewModel, watchers}
       |> Js.Promise.resolve
     );
};

let persist = ({id, log, state} as venture) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let summaryString =
    log |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFileNotEncrypted(
        (id |> VentureId.toString) ++ "/log.json",
        logString,
      )
      |> then_(() => resolve(venture))
    );
  Js.Promise.(
    state.partnerStoragePrefixes
    |> List.fold_left(
         (promise, prefix) =>
           promise
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (id |> VentureId.toString) ++ "/" ++ prefix ++ "/log.json",
                  logString,
                )
              )
           |> then_(() =>
                Blockstack.putFileNotEncrypted(
                  (id |> VentureId.toString)
                  ++ "/"
                  ++ prefix
                  ++ "/summary.json",
                  summaryString,
                )
              ),
         resolve(),
       )
    |> ignore
  );
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (session: Session.Data.t, ~ventureId) => {
  logMessage("Loading venture '" ++ VentureId.toString(ventureId) ++ "'");
  Js.Promise.(
    Blockstack.getFileNotDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) =>
           raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct(session)
         | None => raise(CouldNotLoadVenture)
         }
       )
    |> then_(persist)
  );
};

let join = (session: Session.Data.t, ~userId, ~ventureId) =>
  Js.Promise.(
    Blockstack.getFileFromUser(
      ventureId ++ "/" ++ session.storagePrefix ++ "/log.json",
      ~username=userId,
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.toOption(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) =>
           raw |> Json.parseOrRaise |> EventLog.decode |> reconstruct(session)
         }
       )
    |> then_(persist)
    |> then_(venture =>
         Index.add(
           ~ventureId=venture.id,
           ~ventureName=venture.state.ventureName,
         )
         |> then_(index => resolve((index, venture)))
       )
  );

let getId = ({id}) => id |> VentureId.toString;

let getSummary = ({log}) => log |> EventLog.getSummary;

let getViewModel = ({viewModel}) => viewModel;

module Synchronize = {
  let getPartnerHistoryUrls = ({session, id, state}) =>
    state.partnerIds
    |> List.filter(partnerId => UserId.neq(partnerId, session.userId))
    |> List.map(partnerId =>
         Blockstack.getUserAppFileUrl(
           ~path=(id |> VentureId.toString) ++ "/" ++ session.storagePrefix,
           ~username=partnerId |> UserId.toString,
           ~appOrigin=Location.origin,
         )
       )
    |> Array.of_list
    |> Js.Promise.all;
  type result =
    | Ok(t)
    | Error(t, EventLog.item, Validation.result);
  let exec = (otherLogs, {session, log} as venture) => {
    let newItems = log |> EventLog.findNewItems(otherLogs);
    let ({log, state, wallet, viewModel, watchers}, error) =
      newItems
      |> List.fold_left(
           (
             ({log, watchers, state, wallet, viewModel} as venture, error),
             {event} as item: EventLog.item,
           ) =>
             if (Js.Option.isSome(error)) {
               (venture, error);
             } else {
               switch (item |> Validation.validate(state)) {
               | Ok =>
                 let log = log |> EventLog.appendItem(item);
                 let state = state |> Validation.apply(event);
                 let wallet = wallet |> Wallet.apply(event);
                 let viewModel = viewModel |> ViewModel.apply(event);
                 let watchers =
                   watchers |> Watchers.apply(session, item, log);
                 (
                   {...venture, log, watchers, state, wallet, viewModel},
                   None,
                 );
               | PolicyMissmatch as conflict => (
                   venture,
                   Some(Error(venture, item, conflict)),
                 )
               | PolicyNotFulfilled as conflict => (
                   venture,
                   Some(Error(venture, item, conflict)),
                 )
               /* Ignored validation issues */
               | InvalidIssuer =>
                 logMessage("Invalid issuer detected");
                 (venture, None);
               | BadData(msg) =>
                 logMessage("Bad data in event detected: " ++ msg);
                 (venture, None);
               | UnknownProcessId =>
                 logMessage("Unknown ProcessId detected");
                 (venture, None);
               | DuplicateEndorsement =>
                 logMessage("Duplicate Endorsement detected");
                 (venture, None);
               | DependencyNotMet =>
                 logMessage("Dependency Not Met detected");
                 (venture, None);
               };
             },
           (venture, None),
         );
    Js.Promise.(
      watchers
      |> Watchers.processPending(
           session,
           log,
           applyInternal,
           (state, wallet, viewModel),
         )
      |> then_(((log, (state, wallet, viewModel), watchers)) =>
           {...venture, log, state, wallet, viewModel, watchers} |> persist
         )
      |> then_(p =>
           (
             switch (error) {
             | None => Ok(p)
             | Some(e) => e
             }
           )
           |> resolve
         )
    );
  };
};

let getPartnerHistoryUrls = Synchronize.getPartnerHistoryUrls;

module Cmd = {
  module Create = {
    type result = (Index.t, t);
    let exec = (session: Session.Data.t, ~name as ventureName) => {
      logMessage("Executing 'Create' command");
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.userId,
          ~creatorPubKey=session.issuerKeyPair |> Utils.publicKeyFromKeyPair,
          ~metaPolicy=defaultPolicy,
          ~network=session.network,
        );
      Js.(
        Promise.all2((
          Index.add(~ventureId=ventureCreated.ventureId, ~ventureName),
          make(session, ventureCreated.ventureId)
          |> apply(VentureCreated(ventureCreated))
          |> Promise.then_(persist),
        ))
      );
    };
  };
  module Synchronize = Synchronize;
  module ProposePartner = {
    type result =
      | Ok(t)
      | NoUserInfo;
    let exec = (~prospectId, {session, state} as venture) => {
      logMessage("Executing 'ProposePartner' command");
      Js.Promise.(
        UserInfo.Public.read(~blockstackId=prospectId)
        |> then_(
             fun
             | UserInfo.Public.Ok(info) => {
                 let Event.PartnerProposed(partnerProposal) =
                   Event.makePartnerProposed(
                     ~supporterId=session.userId,
                     ~prospectId,
                     ~prospectPubKey=info.appPubKey,
                     ~policy=
                       state.policies |> List.assoc(Event.Partner.processName),
                   );
                 let Event.CustodianProposed(custodianProposal) =
                   Event.makeCustodianProposed(
                     ~dependsOn=Some(partnerProposal.processId),
                     ~supporterId=session.userId,
                     ~partnerId=prospectId,
                     ~accountIdx=AccountIndex.default,
                     ~policy=
                       state.policies
                       |> List.assoc(Event.Custodian.processName),
                   );
                 venture
                 |> apply(Event.PartnerProposed(partnerProposal))
                 |> then_(
                      apply(Event.CustodianProposed(custodianProposal)),
                    )
                 |> then_(persist)
                 |> then_(p => resolve(Ok(p)));
               }
             | UserInfo.Public.NotFound => resolve(NoUserInfo),
           )
      );
    };
  };
  module EndorsePartner = {
    type result =
      | Ok(t);
    let exec = (~processId, {state, session} as venture) => {
      logMessage("Executing 'EndorsePartner' command");
      let {id: partnerId}: Event.Partner.Data.t =
        state.partnerData |> List.assoc(processId);
      let (custodianProcessId, _) =
        state.custodianData
        |> List.find(((_, data: Event.Custodian.Data.t)) =>
             data.partnerId == partnerId
           );
      Js.Promise.(
        venture
        |> apply(
             Event.makePartnerEndorsed(
               ~processId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(
             apply(
               Event.makeCustodianEndorsed(
                 ~processId=custodianProcessId,
                 ~supporterId=session.userId,
               ),
             ),
           )
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module ExposeIncomeAddress = {
    type result =
      | Ok(string, t);
    let exec = (~accountIdx, {wallet} as venture) => {
      logMessage("Executing 'GetIncomeAddress' command");
      let exposeEvent = wallet |> Wallet.exposeNextIncomeAddress(accountIdx);
      Js.Promise.(
        venture
        |> apply(IncomeAddressExposed(exposeEvent))
        |> then_(persist)
        |> then_(p => resolve(Ok(exposeEvent.address, p)))
      );
    };
  };
  module ProposePayout = {
    type result =
      | Ok(t);
    let exec =
        (~accountIdx, ~destinations, ~fee, {wallet, session} as venture) => {
      logMessage("Executing 'ProposePayout' command");
      Js.Promise.(
        Wallet.preparePayoutTx(session, accountIdx, destinations, fee, wallet)
        |> then_(proposal => venture |> apply(PayoutProposed(proposal)))
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
  module EndorsePayout = {
    type result =
      | Ok(t);
    let exec = (~processId, {session} as venture) => {
      logMessage("Executing 'EndorsePayout' command");
      Js.Promise.(
        venture
        |> apply(
             Event.makePayoutEndorsed(
               ~processId,
               ~supporterId=session.userId,
             ),
           )
        |> then_(persist)
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
};
