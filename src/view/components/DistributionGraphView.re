[%bs.raw {|require('../../assets/css/graph.css')|}];

open PrimitiveTypes;

open D3;

open DistributionGraph;

let width = 800;

let height = 500;

let toData = graph => {
  let links =
    graph.nodes
    |> List.map(((labelId, node)) =>
         switch node {
         | Node.PartnerDistribution({distribution}) =>
           distribution
           |> List.map(((userId, weight)) =>
                {
                  "source": LabelId.toString(labelId),
                  "target": UserId.toString(userId),
                  "weight": weight
                }
              )
         | Node.LabelDistribution({distribution}) =>
           distribution
           |> List.map(((label, weight)) =>
                {
                  "source": LabelId.toString(labelId),
                  "target": LabelId.toString(label),
                  "weight": weight
                }
              )
         }
       )
    |> List.flatten;
  let nodes =
    graph.nodes
    |> List.map(((labelId, node)) =>
         switch node {
         | Node.PartnerDistribution({distribution}) => [
             {
               "id": LabelId.toString(labelId),
               "nodeType": "label",
               "fx": Js.Nullable.null,
               "fy": Js.Nullable.null
             },
             ...distribution
                |> List.map(((userId, _)) =>
                     {
                       "id": UserId.toString(userId),
                       "nodeType": "user",
                       "fx": Js.Nullable.null,
                       "fy": Js.Nullable.null
                     }
                   )
           ]
         | Node.LabelDistribution(_) => [
             {
               "id": LabelId.toString(labelId),
               "nodeType": "label",
               "fx": Js.Nullable.null,
               "fy": Js.Nullable.null
             }
           ]
         }
       )
    |> List.flatten
    |> List.sort_uniq((n1, n2) => String.compare(n1##id, n2##id))
    |> List.map(n =>
         n##id == "root" ?
           {
             "id": "root",
             "nodeType": "label",
             "fx": Js.Nullable.return(10),
             "fy": Js.Nullable.return(height / 2)
           } :
           n
       );
  (nodes |> Array.of_list, links |> Array.of_list);
};

type action =
  | Bla;

type state = {bla: string};

let component = ReasonReact.reducerComponent("DistributionGraph");

let make = (~graph: DistributionGraph.t, _children) => {
  ...component,
  initialState: () => {bla: ""},
  didMount: (_) => {
    let (nodes, links) = toData(graph);
    let simulation =
      Force.(
        Simulation.make(nodes)
        |> Simulation.force(
             "link",
             Link.make(links) |> Link.id(d => d##id) |> Link.distance(200)
           )
        |> Simulation.force(
             "charge",
             ManyBody.make() |> ManyBody.strength(-1000)
           )
        /* |> Simulation.force("collide", Collide.make(30)) */
        |> Simulation.force("center", Center.make(width / 2, height / 2))
      );
    let svg = Selection.make("svg");
    let link =
      Selection.(
        svg
        |> append("g")
        |> attr("class", "links")
        |> selectAll("line")
        |> data(links)
        |> enter()
        |> append("line")
      );
    let node =
      Selection.(
        svg
        |> append("g")
        |> attr("class", "nodes")
        |> selectAll(".node")
        |> data(nodes)
        |> enter()
        |> append("g")
        |> attr("class", d =>
             d##nodeType == "label" ? "labelNode" : "userNode"
           )
      );
    Selection.(node |> append("circle") |> ignore);
    Selection.(
      node
      |> append("text")
      |> attr("dy", -5)
      |> attr("dx", -5)
      |> text(d => d##id)
      |> ignore
    );
    simulation
    |> Force.Simulation.on("tick", () => {
         Selection.(
           link
           |> attr("x1", d => d##source##x)
           |> attr("y1", d => d##source##y)
           |> attr("x2", d => d##target##x)
           |> attr("y2", d => d##target##y)
           |> ignore
         );
         Selection.(
           node
           |> attr("transform", d =>
                "translate(" ++ d##x ++ "," ++ d##y ++ ")"
              )
           |> ignore
         );
       })
    |> ignore;
    /* Js.log(s); */
    ReasonReact.NoUpdate;
  },
  reducer: (action, state: state) =>
    switch action {
    | Bla => ReasonReact.NoUpdate
    },
  render: (_) =>
    <svg width=(string_of_int(width)) height=(string_of_int(height)) />
};
