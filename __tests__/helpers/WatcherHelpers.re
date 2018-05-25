open Jest;

open Expect;

module G = Generators;

module E = G.Event;

module L = G.Log;

module F = Fixtures;

let testWatcherHasCompleted = watcher =>
  test("the watcher has completed", () =>
    expect(watcher#processCompleted()) |> toEqual(true)
  );

let testWatcherHasNotCompleted = watcher =>
  test("the watcher has not completed", () =>
    expect(watcher#processCompleted()) |> toEqual(false)
  );

let testWatcherHasNoEventPending = watcher => {
  testWatcherHasNotCompleted(watcher);
  test("and has no event pending", () =>
    watcher#pendingEvent() |> Js.Option.isNone |> expect |> toEqual(true)
  );
};

let testWatcherHasEventPending =
    (eventName, watcher, expectedIssuer, eventTest) => {
  testWatcherHasNotCompleted(watcher);
  test("and has " ++ eventName ++ " pending", () =>
    watcher#pendingEvent()
    |> Js.Option.getExn
    |> (
      ((issuer, event)) =>
        expect((issuer, eventTest(event)))
        |> toEqual((expectedIssuer, true))
    )
  );
};
