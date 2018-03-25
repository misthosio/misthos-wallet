open PrimitiveTypes;

let logMessage = msg => Js.log("[Venture] - " ++ msg);

module Index = Venture__Index;

module Validation = Venture__Validation;

exception InvalidEvent(Validation.result);

type t = {
  session: Session.Data.t,
  id: ventureId,
  log: EventLog.t,
  state: Validation.state,
  viewModel: ViewModel.t,
  watchers: Watchers.t
};

let make = (session, id) => {
  session,
  id,
  log: EventLog.make(),
  state: Validation.makeState(),
  viewModel: ViewModel.make(),
  watchers: []
};

let applyInternal = (issuer, event, log, (state, viewModel)) => {
  logMessage("Appending event to log:" );
  logMessage(Event.encode(event)|>Json.stringify);
  let (item, log) = log |> EventLog.append(issuer, event);
  switch (item |> Validation.validate(state)) {
  | Ok =>
    let state = state |> Validation.apply(event);
    let viewModel = viewModel |> ViewModel.apply(event);
    (item, log, (state, viewModel));
  /* This should never happen / only incase of an UI input bug!!! */
  | result =>
    logMessage("Event was rejected because of:");
    logMessage(Validation.resultToString(result));
    raise(InvalidEvent(result));
  };
};

let apply = (event, {session, id, log, state, viewModel, watchers}) => {
  let (item, log, (state, viewModel)) =
    applyInternal(session.appKeyPair, event, log, (state, viewModel));
  let (log, (state, viewModel), watchers) =
    watchers
    |> Watchers.applyAndProcessPending(
         session,
         item,
         log,
         applyInternal,
         (state, viewModel)
       );
  {session, id, log, state, viewModel, watchers};
};

let reconstruct = (session, log) => {
  let {viewModel, state} = make(session, VentureId.make());
  let (id, state, viewModel, watchers) =
    log
    |> EventLog.reduce(
         ((id, state, viewModel, watchers), {event} as item) => (
           switch event {
           | VentureCreated({ventureId}) => ventureId
           | _ => id
           },
           state |> Validation.apply(event),
           viewModel |> ViewModel.apply(event),
           watchers |> Watchers.apply(~reconstruct=true, session, item, log)
         ),
         (VentureId.make(), state, viewModel, [])
       );
  let (log, (state, viewModel), watchers) =
    watchers
    |> Watchers.processPending(session, log, applyInternal, (state, viewModel));
  {session, id, log, state, viewModel, watchers};
};

let persist = ({id, log, state} as venture) => {
  let logString = log |> EventLog.encode |> Json.stringify;
  let summaryString =
    log |> EventLog.getSummary |> EventLog.encodeSummary |> Json.stringify;
  let returnPromise =
    Js.Promise.(
      Blockstack.putFile((id |> VentureId.toString) ++ "/log.json", logString)
      |> then_(() => resolve(venture))
    );
  Js.Promise.(
    state.partnerAddresses
    |> List.fold_left(
         (promise, address) =>
           promise
           |> then_(() =>
                Blockstack.putFile(
                  (id |> VentureId.toString) ++ "/" ++ address ++ "/log.json",
                  logString
                )
              )
           |> then_(() =>
                Blockstack.putFile(
                  (id |> VentureId.toString)
                  ++ "/"
                  ++ address
                  ++ "/summary.json",
                  summaryString
                )
              ),
         resolve()
       )
    |> ignore
  );
  returnPromise;
};

let defaultPolicy = Policy.absolute;

let load = (session: Session.Data.t, ~ventureId) =>
  Js.Promise.(
    Blockstack.getFile((ventureId |> VentureId.toString) ++ "/log.json")
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) =>
           resolve(
             raw
             |> Json.parseOrRaise
             |> EventLog.decode
             |> reconstruct(session)
           )
         | None => raise(Not_found)
         }
       )
    |> then_(persist)
  );

let join = (session: Session.Data.t, ~userId, ~ventureId) =>
  Js.Promise.(
    Blockstack.getFileFromUser(
      ventureId ++ "/" ++ session.address ++ "/log.json",
      ~username=userId
    )
    |> catch(_error => raise(Not_found))
    |> then_(nullFile =>
         switch (Js.Nullable.toOption(nullFile)) {
         | None => raise(Not_found)
         | Some(raw) =>
           raw
           |> Json.parseOrRaise
           |> EventLog.decode
           |> reconstruct(session)
           |> persist
         }
       )
    |> then_(venture =>
         Index.add(
           ~ventureId=venture.id,
           ~ventureName=venture.state.ventureName
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
           ~path=(id |> VentureId.toString) ++ "/" ++ session.address,
           ~username=partnerId |> UserId.toString,
           ~appOrigin=Location.origin
         )
       )
    |> Array.of_list
    |> Js.Promise.all;
  type result =
    | Ok(t)
    | Error(t, EventLog.item, Validation.result);
  let exec = (otherLogs, {session, log} as venture) => {
    let newItems = log |> EventLog.findNewItems(otherLogs);
    let ({log, state, viewModel, watchers}, error) =
      newItems
      |> List.fold_left(
           (
             ({log, watchers, state, viewModel} as venture, error),
             {event} as item: EventLog.item
           ) =>
             if (Js.Option.isSome(error)) {
               (venture, error);
             } else {
               switch (item |> Validation.validate(state)) {
               | Ok =>
                 let log = log |> EventLog.appendItem(item);
                 let state = state |> Validation.apply(event);
                 let viewModel = viewModel |> ViewModel.apply(event);
                 let watchers = watchers |> Watchers.apply(session, item, log);
                 ({...venture, log, watchers, state, viewModel}, None);
               | PolicyMissmatch as conflict => (
                   venture,
                   Some(Error(venture, item, conflict))
                 )
               | PolicyNotFulfilled as conflict => (
                   venture,
                   Some(Error(venture, item, conflict))
                 )
               /* Ignored validation issues */
               | InvalidIssuer =>
                 logMessage("Invalid issuer detected");
                 (venture, None);
               | BadData =>
                 logMessage("Bad data in event detected");
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
           (venture, None)
         );
    let (log, (state, viewModel), watchers) =
      watchers
      |> Watchers.processPending(
           session,
           log,
           applyInternal,
           (state, viewModel)
         );
    Js.Promise.(
      {...venture, log, state, viewModel, watchers}
      |> persist
      |> then_(p =>
           (
             switch error {
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
    let exec =
        (session: Session.Data.t, ~name as ventureName) => {
      logMessage("Executing 'Create' command");
      let ventureCreated =
        Event.VentureCreated.make(
          ~ventureName,
          ~creatorId=session.userId,
          ~creatorPubKey=session.appKeyPair |> Utils.publicKeyFromKeyPair,
          ~metaPolicy=defaultPolicy,
        );
      Js.Promise.all2((
        Index.add(~ventureId=ventureCreated.ventureId, ~ventureName),
        make(session, ventureCreated.ventureId)
        |> apply(VentureCreated(ventureCreated))
        |> persist
      ));
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
        |> then_(readResult =>
             switch readResult {
             | UserInfo.Public.Ok(info) =>
               venture
               |> apply(
                    Event.makePartnerProposed(
                      ~supporterId=session.userId,
                      ~prospectId,
                      ~prospectPubKey=info.appPubKey,
                      ~policy=
                        state.policies |> List.assoc(Event.Partner.processName)
                    )
                  )
               |> persist
               |> then_(p => resolve(Ok(p)))
             | UserInfo.Public.NotFound => resolve(NoUserInfo)
             }
           )
      );
    };
  };
  module EndorsePartner = {
    type result =
      | Ok(t);
    let exec = (~processId, {session} as venture) => {
      logMessage("Executing 'EndorsePartner' command");
      Js.Promise.(
        venture
        |> apply(
             Event.makePartnerEndorsed(~processId, ~supporterId=session.userId)
           )
        |> persist
        |> then_(p => resolve(Ok(p)))
      );
    };
  };
};
