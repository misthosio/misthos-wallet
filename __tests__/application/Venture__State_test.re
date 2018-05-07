open Jest;

open Expect;

open PrimitiveTypes;

module G = Generators;

module E = G.Event;

module L = G.Log;

module State = Venture__State;

let constructState = log =>
  log |> L.reduce((s, {event}) => s |> State.apply(event), State.make());

let () = {
  describe("CreateVenture", () => {
    let user1 = G.userSession("user1" |> UserId.fromString);
    let log = L.createVenture(user1);
    let {systemIssuer, ventureName}: Event.VentureCreated.t =
      log |> L.lastEvent |> Event.getVentureCreatedExn;
    let state = log |> constructState;
    test("extract ventureName", () =>
      expect(state |> State.ventureName) |> toEqual(ventureName)
    );
    test("extract systemIssuer", () =>
      expect(state |> State.systemIssuer) |> toEqual(systemIssuer)
    );
  });
  describe("isPartner", () => {
    let (user1, user2, user3) = G.threeUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
      );
    test("tracks current partners", () => {
      let state = log |> constructState;
      expect(
        [user1, user2, user3]
        |> List.map(({userId}: Session.Data.t) =>
             state |> State.isPartner(userId)
           ),
      )
      |> toEqual([true, true, false]);
    });
    test("tracks partner when they are removed", () => {
      let log =
        L.(
          log
          |> withPartner(user3, ~supporters=[user1, user2])
          |> withPartnerRemoved(user2, ~supporters=[user1, user3])
        );
      let state = log |> constructState;
      expect(
        [user1, user2, user3]
        |> List.map(({userId}: Session.Data.t) =>
             state |> State.isPartner(userId)
           ),
      )
      |> toEqual([true, false, true]);
    });
  });
  describe("ProcessMapping", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartnerProposed(~supporter=user1, ~prospect=user2)
      );
    let {processId: partnerProcess}: Event.Partner.Proposed.t =
      log |> L.lastEvent |> Event.getPartnerProposedExn;
    let log =
      L.(log |> withCustodianProposed(~supporter=user1, ~custodian=user2));
    let {processId: custodianProcess}: Event.Custodian.Proposed.t =
      log |> L.lastEvent |> Event.getCustodianProposedExn;
    let state = log |> constructState;
    test("maps a partnerProcess to a custodianProcess", () =>
      expect(
        state |> State.custodianProcessForPartnerProcess(partnerProcess),
      )
      |> toEqual(custodianProcess)
    );
  });
  describe("ProcessMapping", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodian(user2, ~supporters=[user1, user2])
      );
    let custodianAccepted =
      log |> L.lastEvent |> Event.getCustodianAcceptedExn;
    let state = log |> constructState;
    test("Remembers the latest CustodianAccepted events", () =>
      expect(state |> State.custodianAcceptedFor(user2.userId))
      |> toEqual(Some(custodianAccepted))
    );
  });
  describe("RemovalProcessMapping", () => {
    let (user1, user2) = G.twoUserSessions();
    let log =
      L.(
        createVenture(user1)
        |> withFirstPartner(user1)
        |> withPartner(user2, ~supporters=[user1])
        |> withCustodian(user2, ~supporters=[user1, user2])
        |> withCustodianRemovalProposed(~supporter=user1, ~toBeRemoved=user2)
      );
    let {processId: custodianRemovalProcess}: Event.Custodian.Removal.Proposed.t =
      log |> L.lastEvent |> Event.getCustodianRemovalProposedExn;
    let log =
      L.(
        log
        |> withPartnerRemovalProposed(~supporter=user1, ~toBeRemoved=user2)
      );
    let {processId: partnerRemovalProcess}: Event.Partner.Removal.Proposed.t =
      log |> L.lastEvent |> Event.getPartnerRemovalProposedExn;
    let state = log |> constructState;
    test("maps a partnerRemovalProcess to a custodianRemovalProcess", () =>
      expect(
        state
        |> State.custodianRemovalProcessForPartnerRemovalProcess(
             partnerRemovalProcess,
           ),
      )
      |> toEqual(Some(custodianRemovalProcess))
    );
  });
};
