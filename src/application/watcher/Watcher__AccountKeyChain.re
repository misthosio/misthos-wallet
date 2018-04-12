open Event;

open PrimitiveTypes;

open WalletTypes;

let defaultCosignerList = [|0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6|];

type state = {
  systemIssuer: Bitcoin.ECPair.t,
  custodianKeyChains: list((userId, CustodianKeyChain.public)),
  nextKeyChainIdx: accountKeyChainIdx,
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
};

let make = ({data}: AccountCreation.Acceptance.t, log) => {
  let accountIdx = data.accountIdx;
  let process = {
    val state =
      ref({
        custodianKeyChains: [],
        nextKeyChainIdx: AccountKeyChainIndex.first,
        systemIssuer: Bitcoin.ECPair.makeRandom(),
        pendingEvent: None,
      });
    pub receive = ({event}: EventLog.item) =>
      state :=
        (
          switch (event) {
          | VentureCreated({systemIssuer}) => {...state^, systemIssuer}
          | CustodianKeyChainUpdated({keyChain, partnerId})
              when CustodianKeyChain.accountIdx(keyChain) == accountIdx =>
            let custodianKeyChains = [
              (partnerId, keyChain),
              ...state^.custodianKeyChains |> List.remove_assoc(partnerId),
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
                          defaultCosignerList[custodianKeyChains |> List.length],
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
    pub processCompleted = () => false;
    pub pendingEvent = () =>
      state^.pendingEvent |> Utils.mapOption(Js.Promise.resolve)
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
