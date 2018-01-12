module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type t;

let load: string => Js.Promise.t(t);

let getId: t => string;

let getViewModel: t => ViewModel.t;

module Command: {
  let create: (Session.data, string) => Js.Promise.t((t, Index.t));
  let suggestCandidate: (Session.data, string, t) => Js.Promise.t(t);
};
