open Project;

type action =
  | None;

type state = {id: string};

let component = ReasonReact.reducerComponent("Projects");

let make = (~project, _children) => {
  ...component,
  initialState: () => (),
  reducer: (action, _state) =>
    switch action {
    | None => ReasonReact.Update()
    },
  render: _self => {
    let members =
      ReasonReact.arrayToElement(
        Array.of_list(
          Project.getMembers(project)
          |> List.map(m =>
               <li key=m.blockstackId>
                 (ReasonReact.stringToElement(m.blockstackId))
               </li>
             )
        )
      );
    <div>
      <h2> (ReasonReact.stringToElement(Project.getName(project))) </h2>
      (ReasonReact.stringToElement("Members:"))
      <ul> members </ul>
    </div>;
  }
};
