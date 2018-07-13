open Belt;

include ViewCommon;

type t = {
  heading: string,
  paragraphs: array(string),
};

let terms = [|{heading: "aloha", paragraphs: [|"dummy"|]}|];

let hash =
  terms
  |. Array.mapU((. section) =>
       Array.concat([|section.heading|], section.paragraphs)
     )
  |> Array.concatMany
  |> Js.Array.joinWith(" ")
  |> Utils.hash;
