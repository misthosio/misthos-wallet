type project = {name: string};

let indexPath = "/index.json";

let initializeDeals = () =>
  Js.Promise.(Blockstack.putFile(indexPath, "[]") |> then_(() => resolve([])));

let loadDeals = () =>
  Js.Promise.(
    Blockstack.getFile(indexPath)
    |> then_(nullDeals =>
         switch (Js.Nullable.to_opt(nullDeals)) {
         | None => initializeDeals()
         | Some(projects) => resolve([])
         }
       )
  );
