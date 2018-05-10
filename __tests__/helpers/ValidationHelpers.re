open Jest;

open Expect;

module G = Generators;

module E = G.Event;

module L = G.Log;

module Validation = Venture__Validation;

open PrimitiveTypes;

exception TestingInvalidSequence(string);

let constructState = log =>
  log
  |> L.reduce(
       (s, item) =>
         switch (s |. Validation.validate(item)) {
         | Ok => s |> Validation.apply(item)
         | bad =>
           raise(TestingInvalidSequence(bad |> Validation.resultToString))
         },
       Validation.make(),
     );

let testValidationResult = (state, item, expected) => {
  let description = expected |> Validation.resultToString;
  test("valdation should return '" ++ description ++ "'", () =>
    expect(item |> Validation.validate(state) |> Validation.resultToString)
    |> toEqual(description)
  );
};

let testDataValidation =
    (
      dataValidation: ('a, Validation.t) => Validation.result,
      state,
      data: 'a,
      expected,
    ) => {
  let description = expected |> Validation.resultToString;
  test("valdation should return '" ++ description ++ "'", () =>
    expect(state |> dataValidation(data) |> Validation.resultToString)
    |> toEqual(description)
  );
};

let withSystemIssuer =
    (dataValidation: ('a, Validation.t, 'b) => Validation.result, data, state) =>
  dataValidation(data, state, ());

let withIssuer =
    (
      issuer: Session.Data.t,
      dataValidation: ('a, Validation.t, userId) => Validation.result,
      data,
      state,
    ) =>
  dataValidation(data, state, issuer.userId);
