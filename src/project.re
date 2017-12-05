type t = {name: string};

module Serialization = {
  let project = (json) => Json.Decode.{name: json |> field("name", string)};
  let projectsFromJson = (projectsString) =>
    Js.Json.parseExn(projectsString) |> Json.Decode.list(project);
};

let indexPath = "/index.json";

let initializeProjects = () =>
  Js.Promise.(Blockstack.putFile(indexPath, "[]", Js.false_) |> then_(() => resolve([])));

let listAll = () =>
  Js.Promise.(
    Blockstack.getFile(indexPath, Js.false_)
    |> then_(
         (nullProjects) =>
           switch (Js.Nullable.to_opt(nullProjects)) {
           | None => initializeProjects()
           | Some(projects) => resolve(Serialization.projectsFromJson(projects))
           }
       )
  );
