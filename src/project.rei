module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type t;

let createProject: (Session.data, string) => Js.Promise.t((t, Index.t));
