open Belt;

open PrimitiveTypes;

open Event;

type t = {
  eligible: UserId.set,
  currentPartners: UserId.set,
};

let make = eligible => {eligible, currentPartners: UserId.emptySet};

let currentEligible = ({eligible, currentPartners}) =>
  Set.intersect(eligible, currentPartners);

let apply = (event, state) =>
  switch (event) {
  | PartnerAccepted({data: {id}}) => {
      ...state,
      currentPartners: state.currentPartners |. Set.add(id),
    }
  | PartnerRemovalAccepted({data: {id}}) => {
      ...state,
      currentPartners: state.currentPartners |. Set.remove(id),
    }
  | _ => state
  };
