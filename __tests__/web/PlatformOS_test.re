open Jest;
open Expect;

test("iOS iPhone", () =>
  expect(Js.Re.test("blablab - iPhone ashoten", PlatformOS.iOSCheck))
  |> toEqual(true)
);

test("iOS iPad", () =>
  expect(Js.Re.test("blablab - iPad ashoten", PlatformOS.iOSCheck))
  |> toEqual(true)
);

test("iOS nothing", () =>
  expect(Js.Re.test("nothing", PlatformOS.iOSCheck)) |> toEqual(false)
);

test("Android iPad", () =>
  expect(Js.Re.test("blablab - Android ashoten", PlatformOS.androidCheck))
  |> toEqual(true)
);

test("Android nothing", () =>
  expect(Js.Re.test("nothing", PlatformOS.androidCheck)) |> toEqual(false)
);
