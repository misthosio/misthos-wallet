type t;

let make: unit => t;

let increment: (string, t) => t;

let getCounter: (string, t) => int;

let syncClocks: (t, t) => t;

let encode: t => string;

let decode: string => t;
