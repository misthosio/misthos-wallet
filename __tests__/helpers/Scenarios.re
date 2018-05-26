open Belt;

open PrimitiveTypes;

open Jest;

exception CouldNotLoadScenario;

let basePath = "__tests__/scenarios/";

let scenarioSession = Fixtures.threeUserSessionsArray |. Array.getExn(0);

let loadScenario = scenarioName =>
  try (
    Node.Fs.readFileAsUtf8Sync(basePath ++ scenarioName ++ ".json")
    |> Json.parseOrRaise
    |> EventLog.decode
  ) {
  | _ => raise(CouldNotLoadScenario)
  };

let findCurrentUsers =
  EventLog.reduce(
    (users, item) =>
      switch (item.event) {
      | PartnerAccepted({data: {id}}) => users |. Set.add(id)
      | PartnerRemovalAccepted({data: {id}}) => users |. Set.remove(id)
      | _ => users
      },
    UserId.emptySet,
  );

let run = (scenarioName, test) =>
  describe(
    scenarioName,
    () => {
      let (venture, _newItems) =
        loadScenario(scenarioName) |> Venture.reconstruct(scenarioSession);
      let eventLog = venture |> Venture.getEventLog;
      let currentUsers = eventLog |> findCurrentUsers;
      test(
        venture
        |> Venture.getEventLog
        |> ViewModel.init(currentUsers |> Set.toArray |. Array.getExn(0)),
      );
    },
  );
