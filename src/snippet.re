type userData = {. "username": string};

[@bs.module "blockstack"] [@bs.return nullable] external loadUserData : unit => option(userData) =
  "";

let userData = loadUserData();

let name =
  switch userData {
  | None => "None"
  | Some(data) => data##username
  };
