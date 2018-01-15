type project = {name: string};

let indexPath = "/index.json";

let initializeProjects = () =>
  Js.Promise.(Blockstack.putFile(indexPath, "[]") |> then_(() => resolve([])));

let loadProjects = () =>
  Js.Promise.(
    Blockstack.getFile(indexPath)
    |> then_(nullProjects =>
         switch (Js.Nullable.to_opt(nullProjects)) {
         | None => initializeProjects()
         | Some(projects) => resolve([])
         }
       )
  );
