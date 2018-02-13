open PrimitiveTypes;

module LabelDistribution = {
  type t = {
    labelId,
    distribution: list((labelId, int))
  };
  let encode = ({labelId, distribution}) =>
    Json.Encode.(
      object_([
        ("type", string("LabelDistribution")),
        ("labelId", LabelId.encode(labelId)),
        ("distribution", list(pair(LabelId.encode, int), distribution))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      labelId: raw |> field("labelId", LabelId.decode),
      distribution:
        raw |> field("distribution", list(pair(LabelId.decode, int)))
    };
};

module PartnerDistribution = {
  type t = {
    labelId,
    distribution: list((userId, int))
  };
  let encode = ({labelId, distribution}) =>
    Json.Encode.(
      object_([
        ("type", string("PartnerDistribution")),
        ("labelId", LabelId.encode(labelId)),
        ("distribution", list(pair(UserId.encode, int), distribution))
      ])
    );
  let decode = raw =>
    Json.Decode.{
      labelId: raw |> field("labelId", LabelId.decode),
      distribution:
        raw |> field("distribution", list(pair(UserId.decode, int)))
    };
};

module Node = {
  type t =
    | PartnerDistribution(PartnerDistribution.t)
    | LabelDistribution(LabelDistribution.t);
  let encode = node =>
    switch node {
    | PartnerDistribution(node) => PartnerDistribution.encode(node)
    | LabelDistribution(node) => LabelDistribution.encode(node)
    };
  exception UnknownNodeType(Js.Json.t);
  let decode = raw => {
    let type_ = raw |> Json.Decode.(field("type", string));
    switch type_ {
    | "PartnerDistribution" =>
      PartnerDistribution(PartnerDistribution.decode(raw))
    | "LabelDistribution" => LabelDistribution(LabelDistribution.decode(raw))
    | _ => raise(UnknownNodeType(raw))
    };
  };
};

type t = {nodes: list((labelId, Node.t))};

let encode = graph =>
  Json.Encode.list(Node.encode, graph.nodes |> List.map(snd));

let decode = raw =>
  Json.Decode.{
    nodes:
      list(Node.decode, raw)
      |> List.map(n =>
           (
             switch n {
             | Node.PartnerDistribution(d) => d.labelId
             | Node.LabelDistribution(d) => d.labelId
             },
             n
           )
         )
  };

let rootLabelId = LabelId.fromString("root");

let make = (userId, initialLabelIds) => {
  let leafNodes =
    initialLabelIds
    |> List.map(labelId =>
         (
           labelId,
           Node.PartnerDistribution({labelId, distribution: [(userId, 100)]})
         )
       );
  let rootNode = (
    rootLabelId,
    Node.LabelDistribution({
      labelId: rootLabelId,
      distribution: initialLabelIds |> List.map(labelId => (labelId, 25))
    })
  );
  {nodes: [rootNode, ...leafNodes]};
};
