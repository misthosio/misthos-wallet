open PrimitiveTypes;

exception CouldNotLoadVenture;

let loadVenture = ventureId =>
  Js.Promise.(
    Blockstack.getFileDecrypted(
      (ventureId |> VentureId.toString) ++ "/log.json",
    )
    |> then_(nullLog =>
         switch (Js.Nullable.toOption(nullLog)) {
         | Some(raw) => raw |> Json.parseOrRaise |> EventLog.decode |> resolve
         | None => raise(CouldNotLoadVenture)
         }
       )
  );

let logMessage = (label, msg) => Js.log(label ++ " - " ++ msg);

let logError = (label, error) => {
  Js.Console.error(label ++ " - Encountered an unhandled exception");
  Js.Console.error(error);
};

let catchAndLogError = (label, promise) =>
  promise
  |> Js.Promise.catch(err => {
       logError(label, err);
       Js.Promise.resolve();
     })
  |> ignore;
