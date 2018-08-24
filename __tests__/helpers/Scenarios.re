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
  | err =>
    Js.log(err);
    raise(CouldNotLoadScenario);
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

let run = (scenarioName, scenarioTest) =>
  describe(
    scenarioName,
    () => {
      let loadedLog = loadScenario(scenarioName);
      test(
        "Integrity of "
        ++ string_of_int(loadedLog |> EventLog.length)
        ++ " items is intact",
        () => {
          let newItems =
            EventLog.findNewItems(~other=loadedLog, EventLog.make());
          Expect.expect(newItems |> Array.length)
          |> Expect.toEqual(loadedLog |> EventLog.length);
        },
      );
      let (venture, newItems) =
        loadedLog |> Venture.reconstruct(scenarioSession);
      scenarioTest(venture, newItems);
    },
  );
let runWithView = (scenarioName, scenarioTest) =>
  describe(
    scenarioName,
    () => {
      let loadedLog = loadScenario(scenarioName);
      let (venture, _) = loadedLog |> Venture.reconstruct(scenarioSession);
      venture
      |> Venture.getEventLog
      |> ViewModel.init(UserId.fromString("misthosio.id"))
      |> scenarioTest;
    },
  );
