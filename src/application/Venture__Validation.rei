type t;

let make: unit => t;

let apply: (EventLog.item, t) => t;

type result =
  | Ok
  | Ignore
  | InvalidIssuer
  | UnknownProcessId
  | AlreadyEndorsed
  | PolicyMissmatch
  | PolicyNotFulfilled
  | DependencyNotMet
  | BadData(string);

let resultToString: result => string;

let validate: (t, EventLog.item) => result;
