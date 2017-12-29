type t = {name: string};

type index = list(t);

module Decode = {
  let project = (json) => Json.Decode.{name: json |> field("name", string)};
  let index = (indexString) => Js.Json.parseExn(indexString) |> Json.Decode.list(project);
};

module Encode = {
  let project = (project) => Json.Encode.(object_([("name", string(project.name))]));
  let index = (index) => Json.Encode.list(project, index) |> Json.stringify;
};

let indexPath = "index.json";

let persistIndex = (index) => Blockstack.putFile(indexPath, Encode.index(index), Js.false_);

let loadIndex = () =>
  Js.Promise.(
    Blockstack.getFile(indexPath, Js.false_)
    |> then_(
         (nullProjects) =>
           switch (Js.Nullable.to_opt(nullProjects)) {
           | None => persistIndex([]) |> then_(() => resolve([]))
           | Some(index) => resolve(Decode.index(index))
           }
       )
  );

let createProject = (name) => {
  let project = {name: name};
  Js.Promise.(
    loadIndex()
    |> then_(
         (index) => {
           let newIndex = [project, ...index];
           persistIndex(newIndex) |> then_(() => resolve(newIndex))
         }
       )
  )
};
