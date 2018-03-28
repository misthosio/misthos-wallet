open Event;

open PrimitiveTypes;

open WalletTypes;

let defaultCosignerList = [|0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6|];

type state = {
  systemIssuer: Bitcoin.ECPair.t,
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
  nextKeyChainIndex: accountKeyChainIdx,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t))
};

let make = ({data}: AccountCreation.Acceptance.t, log) => {
  let accountIndex = data.accountIndex;
  let process = {
    val state =
      ref({
        custodianKeyChains: [],
        nextKeyChainIndex: AccountKeyChainIndex.first,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        pendingEvent: None
      });
    pub receive = ({event}: EventLog.item) =>
      state :=
        (
          switch event {
          | VentureCreated({systemIssuer}) => {...state^, systemIssuer}
          | CustodianKeyChainUpdated({keyChain, partnerId})
              when CustodianKeyChain.accountIndex(keyChain) == accountIndex =>
            let custodianKeyChains = [
              (partnerId, keyChain),
              ...state^.custodianKeyChains |> List.remove_assoc(partnerId)
            ];
            {
              ...state^,
              custodianKeyChains,
              pendingEvent:
                Some((
                  state^.systemIssuer,
                  AccountKeyChainUpdated(
                    AccountKeyChainUpdated.make(
                      ~accountIndex,
                      ~keyChainIndex=state^.nextKeyChainIndex,
                      ~keyChain=
                        AccountKeyChain.make(
                          defaultCosignerList[custodianKeyChains |> List.length],
                          custodianKeyChains
                        )
                    )
                  )
                ))
            };
          | AccountKeyChainUpdated({accountIndex as aIdx})
              when aIdx == accountIndex => {
              ...state^,
              pendingEvent: None,
              nextKeyChainIndex:
                state^.nextKeyChainIndex |> AccountKeyChainIndex.next
            }
          | _ => state^
          }
        );
    pub processCompleted = () => false;
    pub pendingEvent = () => state^.pendingEvent
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
