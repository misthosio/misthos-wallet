module type Item = {
  type t;
  let getType: t => string;
  let encode: t => string;
  let decode: (string, ~type_: string) => t;
  type validatorState;
  let canKeyWitnessItem: (validatorState, t, string) => bool;
  let canKeySignItem: (validatorState, t, string) => bool;
  let validate: (validatorState, t) => (bool, validatorState);
};

/* module type MakeType = */
/*   (Payload: Item) => */
/*   { */
/*     type t; */
/*     /1* type verified; *1/ */
/*     /1* type validated; *1/ */
/*     /1* let append: (t, Payload.t) => t; *1/ */
/*     /1* let verify: (Payload.verifierState, t) => verified; *1/ */
/*     /1* let validate: (Payload.validatorState, verified) => validated; *1/ */
/*     /1* let merge: (verified, list(t)) => verified; *1/ */
/*     /1* let encode: verified => string; *1/ */
/*     /1* let decode: string => t; *1/ */
/*   }; */
exception Illegal;

module Make:
  (Payload: Item) =>
  {
    type t;
    type unvalidated;
    type validatorState = Payload.validatorState;
    let make: validatorState => t;
    let validate: (validatorState, unvalidated) => t;
    let append: (Payload.t, Bitcoin.ECPair.t, t) => t;
    let reduce: (('s, Payload.t) => 's, 's, t) => 's;
    let merge: (t, list(unvalidated)) => t;
    let encode: t => string;
    let decode: string => unvalidated;
  };
