open Belt;

open PrimitiveTypes;

type proposal('a) = {
  processId,
  dependsOnProposals: ProcessId.set,
  dependsOnCompletions: ProcessId.set,
  eligibleWhenProposing: UserId.set,
  proposerId: userId,
  policy: Policy.t,
  data: 'a,
};

type rejection = {
  processId,
  rejectorId: userId,
};

type endorsement = {
  processId,
  supporterId: userId,
};

type acceptance('a) = {
  processId,
  dependsOnCompletions: ProcessId.set,
  data: 'a,
};

type denial = {processId};

module type EventData = {
  type t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

module type ProposedEvent =
  (Data: EventData) =>
  {
    type t = proposal(Data.t);
    let make:
      (
        ~dependsOnProposals: ProcessId.set=?,
        ~dependsOnCompletions: ProcessId.set=?,
        ~eligibleWhenProposing: UserId.set,
        ~proposerId: userId,
        ~policy: Policy.t,
        Data.t
      ) =>
      t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeProposal = (name: string) : (module ProposedEvent) =>
  (module
   (Data: EventData) => {
     type t = proposal(Data.t);
     let make =
         (
           ~dependsOnProposals=ProcessId.emptySet,
           ~dependsOnCompletions=ProcessId.emptySet,
           ~eligibleWhenProposing,
           ~proposerId,
           ~policy,
           data,
         ) => {
       processId: ProcessId.make(),
       eligibleWhenProposing,
       dependsOnProposals,
       dependsOnCompletions,
       proposerId,
       policy,
       data,
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           (
             "dependsOnProposals",
             array(ProcessId.encode, event.dependsOnProposals |> Set.toArray),
           ),
           (
             "dependsOnCompletions",
             array(
               ProcessId.encode,
               event.dependsOnCompletions |> Set.toArray,
             ),
           ),
           (
             "eligibleWhenProposing",
             array(UserId.encode, event.eligibleWhenProposing |> Set.toArray),
           ),
           ("proposerId", UserId.encode(event.proposerId)),
           ("policy", Policy.encode(event.policy)),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOnProposals:
           raw
           |> field("dependsOnProposals", array(ProcessId.decode))
           |> Set.mergeMany(ProcessId.emptySet),
         dependsOnCompletions:
           raw
           |> field("dependsOnCompletions", array(ProcessId.decode))
           |> Set.mergeMany(ProcessId.emptySet),
         eligibleWhenProposing:
           raw
           |> field("eligibleWhenProposing", array(UserId.decode))
           |> Set.mergeMany(UserId.emptySet),
         proposerId: raw |> field("proposerId", UserId.decode),
         policy: raw |> field("policy", Policy.decode),
         data: raw |> field("data", Data.decode),
       };
   });

module type RejectedEvent = {
  type t = rejection;
  let make: (~processId: processId, ~rejectorId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeRejection = (name: string) : (module RejectedEvent) =>
  (module
   {
     type t = rejection;
     let make = (~processId, ~rejectorId) => {processId, rejectorId};
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("rejectorId", UserId.encode(event.rejectorId)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         rejectorId: raw |> field("rejectorId", UserId.decode),
       };
   });

module type EndorsedEvent = {
  type t = endorsement;
  let make: (~processId: processId, ~supporterId: userId) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeEndorsement = (name: string) : (module EndorsedEvent) =>
  (module
   {
     type t = endorsement;
     let make = (~processId, ~supporterId) => {processId, supporterId};
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           ("supporterId", UserId.encode(event.supporterId)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         supporterId: raw |> field("supporterId", UserId.decode),
       };
   });

module type AcceptedEvent =
  (Data: EventData) =>
  {
    type t = acceptance(Data.t);
    let fromProposal: proposal(Data.t) => t;
    let encode: t => Js.Json.t;
    let decode: Js.Json.t => t;
  };

let makeAcceptance = (name: string) : (module AcceptedEvent) =>
  (module
   (Data: EventData) => {
     type t = acceptance(Data.t);
     let fromProposal =
         (
           {dependsOnProposals, dependsOnCompletions, processId, data}:
             proposal(Data.t),
         ) => {
       dependsOnCompletions:
         dependsOnProposals |> Set.union(dependsOnCompletions),
       processId,
       data,
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
           (
             "dependsOnCompletions",
             array(
               ProcessId.encode,
               event.dependsOnCompletions |> Set.toArray,
             ),
           ),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOnCompletions:
           raw
           |> field("dependsOnCompletions", array(ProcessId.decode))
           |> Set.mergeMany(ProcessId.emptySet),
         data: raw |> field("data", Data.decode),
       };
   });

module type DeniedEvent = {
  type t = denial;
  let fromProposal: proposal('a) => t;
  let encode: t => Js.Json.t;
  let decode: Js.Json.t => t;
};

let makeDenial = (name: string) : (module DeniedEvent) =>
  (module
   {
     type t = denial;
     let fromProposal = ({processId}: proposal('a)) => {
       processId: processId,
     };
     let encode = (event: t) =>
       Json.Encode.(
         object_([
           ("type", string(name)),
           ("processId", ProcessId.encode(event.processId)),
         ])
       );
     let decode = raw =>
       Json.Decode.{processId: raw |> field("processId", ProcessId.decode)};
   });

module type Process =
  (Data: EventData) =>
  {
    let processName: string;
    let dataEq: (Data.t, Data.t) => bool;
    module Proposed: {
      type t = proposal(Data.t);
      let make:
        (
          ~dependsOnProposals: ProcessId.set=?,
          ~dependsOnCompletions: ProcessId.set=?,
          ~eligibleWhenProposing: UserId.set,
          ~proposerId: userId,
          ~policy: Policy.t,
          Data.t
        ) =>
        t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Rejected: {
      type t = rejection;
      let make: (~processId: processId, ~rejectorId: userId) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Endorsed: {
      type t = endorsement;
      let make: (~processId: processId, ~supporterId: userId) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Accepted: {
      type t = acceptance(Data.t);
      let fromProposal: proposal(Data.t) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
    module Denied: {
      type t = denial;
      let fromProposal: proposal('a) => t;
      let encode: t => Js.Json.t;
      let decode: Js.Json.t => t;
    };
  };

let makeProcess = (name: string) : (module Process) =>
  (module
   (Data: EventData) => {
     let processName = name ++ "ApprovalProcess";
     module Proposed = (val makeProposal(name ++ "Proposed"))(Data);
     module Rejected = (val makeRejection(name ++ "Rejected"));
     module Endorsed = (val makeEndorsement(name ++ "Endorsed"));
     module Accepted = (val makeAcceptance(name ++ "Accepted"))(Data);
     module Denied = (val makeDenial(name ++ "Denied"));
     let dataEq = (dataA, dataB) =>
       Data.encode(dataA) == Data.encode(dataB);
   });
