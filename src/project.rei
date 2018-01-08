module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type state = {
  id: string,
  name: string
};

type t;

let load: string => Js.Promise.t(t);

let createProject: (Session.data, string) => Js.Promise.t((t, Index.t));

let getState: t => state;
