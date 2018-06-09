open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  systemIssuer: Bitcoin.ECPair.t,
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
  identifiedKeyChains: list((AccountKeyChain.Identifier.t, int)),
  identifiedEvent: option((Bitcoin.ECPair.t, Event.t)),
  activatedEvent: option((Bitcoin.ECPair.t, Event.t)),
  waitingForIdentification: string,
  active: bool,
};

let make =
    (
      {userId: localUserId, issuerKeyPair}: SessionData.t,
      {data: {accountIdx}}: AccountCreation.Accepted.t,
      log,
    ) => {
  let identifiedEvent = (keyChains, state) => {
    let event =
      AccountKeyChainIdentified.make(
        ~keyChain=AccountKeyChain.make(accountIdx, keyChains),
      );
    let identifier = event.keyChain.identifier;
    state.identifiedKeyChains |> List.mem_assoc(identifier) ?
      (identifier, state.identifiedKeyChains, None) :
      (
        identifier,
        [(identifier, 0), ...state.identifiedKeyChains],
        Some((state.systemIssuer, AccountKeyChainIdentified(event))),
      );
  };
  let activatedEvent = (identifier, identifiedKeyChains) =>
    Some((
      issuerKeyPair,
      AccountKeyChainActivated(
        AccountKeyChainActivated.make(
          ~accountIdx,
          ~custodianId=localUserId,
          ~identifier,
          ~sequence=identifiedKeyChains |> List.assoc(identifier),
        ),
      ),
    ));
  let process = {
    val state =
      ref({
        custodianKeyChains: [],
        identifiedKeyChains: [],
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        waitingForIdentification: "",
        identifiedEvent: None,
        activatedEvent: None,
        active: false,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | VentureCreated({systemIssuer}) => {...state^, systemIssuer}
          | PartnerAccepted({data: {id}}) when UserId.eq(id, localUserId) => {
              ...state^,
              active: true,
            }
          | PartnerRemovalAccepted({data: {id}})
              when UserId.eq(id, localUserId) => {
              ...state^,
              active: false,
            }
          | CustodianRemovalAccepted({
              data: {custodianId, accountIdx as removedAccount},
            })
              when AccountIndex.eq(removedAccount, accountIdx) =>
            try (
              {
                let custodianKeyChains =
                  state^.custodianKeyChains |> List.remove_assoc(custodianId);
                let (identifier, identifiedKeyChains, event) =
                  state^ |> identifiedEvent(custodianKeyChains);
                {
                  ...state^,
                  waitingForIdentification: identifier,
                  custodianKeyChains,
                  identifiedKeyChains,
                  activatedEvent:
                    activatedEvent(identifier, identifiedKeyChains),
                  identifiedEvent: event,
                };
              }
            ) {
            | Not_found => state^
            }
          | CustodianKeyChainUpdated({keyChain, custodianId})
              when CustodianKeyChain.accountIdx(keyChain) == accountIdx =>
            let custodianKeyChains = [
              (custodianId, keyChain),
              ...state^.custodianKeyChains |> List.remove_assoc(custodianId),
            ];
            let (identifier, identifiedKeyChains, event) =
              state^ |> identifiedEvent(custodianKeyChains);
            {
              ...state^,
              waitingForIdentification: identifier,
              custodianKeyChains,
              identifiedKeyChains,
              activatedEvent: activatedEvent(identifier, identifiedKeyChains),
              identifiedEvent: event,
            };
          | AccountKeyChainActivated({
              accountIdx: aIdx,
              custodianId,
              identifier,
            })
              when
                AccountIndex.eq(aIdx, accountIdx)
                && UserId.eq(custodianId, localUserId) => {
              ...state^,
              activatedEvent: None,
              identifiedKeyChains: [
                (
                  identifier,
                  (state^.identifiedKeyChains |> List.assoc(identifier)) + 1,
                ),
                ...state^.identifiedKeyChains |> List.remove_assoc(identifier),
              ],
            }
          | AccountKeyChainIdentified({keyChain})
              when
                keyChain.accountIdx == accountIdx
                && keyChain.identifier == state^.waitingForIdentification => {
              ...state^,
              waitingForIdentification: "",
              identifiedEvent: None,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () => false;
    pub pendingEvent = () =>
      state^.active ?
        switch (state^.identifiedEvent) {
        | Some(_) => state^.identifiedEvent
        | None => state^.activatedEvent
        } :
        None
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
