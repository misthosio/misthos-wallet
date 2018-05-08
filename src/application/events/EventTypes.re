open PrimitiveTypes;

type proposal('a) = {
  processId,
  dependsOnProposals: list(processId),
  dependsOnCompletions: list(processId),
  supporterId: userId,
  policy: Policy.t,
  data: 'a,
};

type endorsement = {
  processId,
  supporterId: userId,
};

type acceptance('a) = {
  processId,
  dependsOnCompletions: list(processId),
  data: 'a,
};

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
        ~dependsOnProposals: list(processId)=?,
        ~dependsOnCompletions: list(processId)=?,
        ~supporterId: userId,
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
           ~dependsOnProposals=[],
           ~dependsOnCompletions=[],
           ~supporterId,
           ~policy,
           data,
         ) => {
       processId: ProcessId.make(),
       dependsOnProposals,
       dependsOnCompletions,
       supporterId,
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
             list(ProcessId.encode, event.dependsOnProposals),
           ),
           (
             "dependsOnCompletions",
             list(ProcessId.encode, event.dependsOnCompletions),
           ),
           ("supporterId", UserId.encode(event.supporterId)),
           ("policy", Policy.encode(event.policy)),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOnProposals:
           raw |> field("dependsOnProposals", list(ProcessId.decode)),
         dependsOnCompletions:
           raw |> field("dependsOnCompletions", list(ProcessId.decode)),
         supporterId: raw |> field("supporterId", UserId.decode),
         policy: raw |> field("policy", Policy.decode),
         data: raw |> field("data", Data.decode),
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
         dependsOnProposals |> List.append(dependsOnCompletions),
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
             list(ProcessId.encode, event.dependsOnCompletions),
           ),
           ("data", Data.encode(event.data)),
         ])
       );
     let decode = raw =>
       Json.Decode.{
         processId: raw |> field("processId", ProcessId.decode),
         dependsOnCompletions:
           raw |> field("dependsOnCompletions", list(ProcessId.decode)),
         data: raw |> field("data", Data.decode),
       };
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
          ~dependsOnProposals: list(processId)=?,
          ~dependsOnCompletions: list(processId)=?,
          ~supporterId: userId,
          ~policy: Policy.t,
          Data.t
        ) =>
        t;
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
  };

let makeProcess = (name: string) : (module Process) =>
  (module
   (Data: EventData) => {
     let processName = name ++ "ApprovalProcess";
     module Proposed = (val makeProposal(name ++ "Proposed"))(Data);
     module Endorsed = (val makeEndorsement(name ++ "Endorsed"));
     module Accepted = (val makeAcceptance(name ++ "Accepted"))(Data);
     let dataEq = (dataA, dataB) =>
       Data.encode(dataA) == Data.encode(dataB);
   });
