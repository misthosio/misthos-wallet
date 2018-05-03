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
