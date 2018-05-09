open Event;

open PrimitiveTypes;

open WalletTypes;

type state = {
  systemIssuer: Bitcoin.ECPair.t,
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
  nextKeyChainIdx: accountKeyChainIdx,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
};

let make = ({data}: AccountCreation.Accepted.t, log) => {
  let accountIdx = data.accountIdx;
  let process = {
    val state =
      ref({
        custodianKeyChains: [],
        nextKeyChainIdx: AccountKeyChainIndex.first,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        pendingEvent: None,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | VentureCreated({systemIssuer}) => {...state^, systemIssuer}
          | CustodianRemovalAccepted({
              data: {custodianId, accountIdx as removedAccount},
            })
              when AccountIndex.eq(removedAccount, accountIdx) =>
            try (
              {
                let custodianKeyChains =
                  state^.custodianKeyChains |> List.remove_assoc(custodianId);
                {
                  ...state^,
                  custodianKeyChains,
                  pendingEvent:
                    Some((
                      state^.systemIssuer,
                      AccountKeyChainUpdated(
                        AccountKeyChainUpdated.make(
                          ~keyChain=
                            AccountKeyChain.make(
                              accountIdx,
                              state^.nextKeyChainIdx,
                              custodianKeyChains,
                            ),
                        ),
                      ),
                    )),
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
            {
              ...state^,
              custodianKeyChains,
              pendingEvent:
                Some((
                  state^.systemIssuer,
                  AccountKeyChainUpdated(
                    AccountKeyChainUpdated.make(
                      ~keyChain=
                        AccountKeyChain.make(
                          accountIdx,
                          state^.nextKeyChainIdx,
                          custodianKeyChains,
                        ),
                    ),
                  ),
                )),
            };
          | AccountKeyChainUpdated({keyChain})
              when keyChain.accountIdx == accountIdx => {
              ...state^,
              pendingEvent: None,
              nextKeyChainIdx:
                state^.nextKeyChainIdx |> AccountKeyChainIndex.next,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () => false;
    pub pendingEvent = () =>
      state^.pendingEvent |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
