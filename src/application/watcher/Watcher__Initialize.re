open Event;

open PrimitiveTypes;

type progress =
  | ProposePartner;

type state = {
  systemIssuer: Bitcoin.ECPair.t,
  creatorKeyPair: Bitcoin.ECPair.t,
  creatorId: userId,
  progress
};

let make =
    (
      {creatorId, creatorPubKey, systemIssuer, metaPolicy}: VentureCreated.t,
      log
    ) => {
  let process = {
    val state =
      ref({
        creatorKeyPair: Bitcoin.ECPair.makeRandom(),
        creatorId,
        systemIssuer,
        progress: ProposePartner
      });
    val completed = ref(false);
    val result = ref(None);
    /* Some( */
    /*   Event.makePartnerProposed( */
    /*     ~supporterId=creatorId, */
    /*     ~prospectId=creatorId, */
    /*     ~prospectPubKey=creatorPubKey, */
    /*     ~policy=metaPolicy */
    /*   ) */
    pub receive = ({event}: EventLog.item) => {
      state :=
        (
          switch event {
          | _ => state^
          }
        );
      result := None;
    };
    pub processCompleted = () => completed^;
    pub pendingEvent = () => result^
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
