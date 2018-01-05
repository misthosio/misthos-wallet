type t;

let make: unit => t;

let increment: (string, t) => t;

let getCounter: (string, t) => int;

let syncClocks: (t, t) => t;

let encode: t => Js.Json.t;

let decode: Js.Json.t => t;
