module Index: {
  type item = {
    id: string,
    name: string
  };
  type t = list(item);
  let load: unit => Js.Promise.t(t);
};

type pubKey = string;

type member = {
  blockstackId: string,
  pubKey,
  address: string,
  storageUrlPrefix: string
};

type candidate = {
  member,
  approval: list(pubKey)
};

type state = {
  id: string,
  name: string,
  members: list((pubKey, member)),
  candidates: list(candidate)
};

type t;

let load: string => Js.Promise.t(t);

let create: (Session.data, string) => Js.Promise.t((t, Index.t));

let suggestCandidate: (Session.data, string, t) => Js.Promise.t(t);

let getId: t => string;

let getName: t => string;

let getMembers: t => list(member);
