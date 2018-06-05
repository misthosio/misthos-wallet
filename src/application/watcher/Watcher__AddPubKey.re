open Event;

open PrimitiveTypes;

type state = {
  pendingEvent: option((Bitcoin.ECPair.t, Event.t)),
  completed: bool,
};

let make =
    (
      {userId, issuerKeyPair}: Session.Data.t,
      {data: {id, pubKey}}: Partner.Accepted.t,
      log,
    ) => {
  let process = {
    val state =
      ref({
        pendingEvent:
          UserId.eq(id, userId) && pubKey |> Js.Option.isNone ?
            Some((
              issuerKeyPair,
              PartnerPubKeyAdded(
                Event.Partner.PubKeyAdded.make(
                  ~partnerId=userId,
                  ~pubKey=issuerKeyPair |> Utils.publicKeyFromKeyPair,
                ),
              ),
            )) :
            None,
        completed: pubKey |> Js.Option.isSome,
      });
    pub receive = ({event}: EventLog.item) => {
      let _ignoreThisWarning = this;
      state :=
        (
          switch (event) {
          | PartnerPubKeyAdded({partnerId}) when UserId.eq(partnerId, id) => {
              ...state^,
              completed: true,
            }
          | _ => state^
          }
        );
    };
    pub processCompleted = () => state^.completed;
    pub pendingEvent = () => state^.pendingEvent
  };
  log |> EventLog.reduce((_, item) => process#receive(item), ());
  process;
};
