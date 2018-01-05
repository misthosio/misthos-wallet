type t = list((string, int));

let make = () => [];

let increment = (label, clock) => {
  let entry =
    try (clock |> List.assoc(label)) {
    | Not_found => 0
    };
  [(label, entry + 1), ...clock |> List.remove_assoc(label)];
};

let getCounter = (label, clock) =>
  try (clock |> List.assoc(label)) {
  | Not_found => 0
  };

let syncClocks = (clockA, clockB) => {
  let state = ([], clockB);
  let (result, rest) =
    clockA
    |> List.fold_left(
         ((current, rest), (label, value)) => (
           [(label, max(value, getCounter(label, rest))), ...current],
           List.remove_assoc(label, rest)
         ),
         state
       );
  rest @ result;
};

let encode = clock =>
  clock |> Json.Encode.(list(pair(string, int))) |> Json.stringify;

let decode = raw =>
  Json.parseOrRaise(raw) |> Json.Decode.(list(pair(string, int)));
