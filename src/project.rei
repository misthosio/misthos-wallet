module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type member = {
  blockstackId: string,
  appPublicKey: string,
  address: string,
  storageUrlPrefix: string
};

type state = {
  id: string,
  name: string,
  members: list(member)
};

type t;

let load: string => Js.Promise.t(t);

let create: (Session.data, string) => Js.Promise.t((t, Index.t));

let getState: t => state;
