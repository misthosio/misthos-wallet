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

let run = (~skipIntegrity=false, scenarioName, scenarioTest) =>
  describe(
    scenarioName,
    () => {
      let loadedLog = loadScenario(scenarioName);
      if (! skipIntegrity) {
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
      };
      let (venture, _newItems) =
        loadedLog |> Venture.reconstruct(scenarioSession);
      scenarioTest(venture);
    },
  );
