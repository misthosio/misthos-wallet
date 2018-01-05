open Jest;

open Expect;

open VectorClock;

let () =
  describe("vectorClock", () => {
    test("increment", () => {
      let clock = make();
      let clock = clock |> increment("a");
      expect(clock |> getCounter("a")) |> toBe(1);
    });
    test("sync", () => {
      let clockA =
        make()
        |> increment("a")
        |> increment("b")
        |> increment("a")
        |> increment("c");
      let clockB =
        make()
        |> increment("a")
        |> increment("b")
        |> increment("b")
        |> increment("d");
      let result = syncClocks(clockA, clockB);
      let counters = [
        getCounter("a", result),
        getCounter("b", result),
        getCounter("c", result),
        getCounter("d", result)
      ];
      expect(counters) |> toEqual([2, 2, 1, 1]);
    });
    test("encode/decode", () => {
      let clock = make();
      let clock = clock |> increment("a");
      expect(clock |> encode |> decode) |> toEqual(clock);
    });
  });
